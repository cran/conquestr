// Fast readers for 'ACER ConQuest' binary files (system files and rout files).
//
// The whole file is held in memory by a `CqReader` (an external pointer with
// class "cqReader") and read sequentially with a cursor. Each reader returns
// exactly the same R structure as the original R implementation in
// R/ReadConQuestLibrary.R (lists of scalars, matrices filled row-wise, the same
// element names and the same types), so downstream code is unaffected.
//
// All values in the file are little-endian. Bytes are assembled explicitly so
// the code is independent of the host byte order.

#include <Rcpp.h>
#include <cstdint>
#include <cstring>
#include <string>
#include <vector>

using namespace Rcpp;

namespace {

class CqReader {
public:
  explicit CqReader(const RawVector& x) : buf_(x.begin(), x.end()), pos_(0) {}

  int readInt() {
    need(4);
    const unsigned char* b = &buf_[pos_];
    uint32_t u = static_cast<uint32_t>(b[0]) |
      (static_cast<uint32_t>(b[1]) << 8) |
      (static_cast<uint32_t>(b[2]) << 16) |
      (static_cast<uint32_t>(b[3]) << 24);
    pos_ += 4;
    int32_t v;
    std::memcpy(&v, &u, sizeof v);
    return v;
  }

  double readDouble() {
    need(8);
    uint64_t u = 0;
    for (int k = 7; k >= 0; --k) u = (u << 8) | buf_[pos_ + k];
    pos_ += 8;
    double d;
    std::memcpy(&d, &u, sizeof d);
    return d;
  }

  // readBin(what = logical(), size = 1) stores the signed byte value as is
  int readBoolByte() {
    need(1);
    return static_cast<int>(static_cast<signed char>(buf_[pos_++]));
  }

  const unsigned char* readBytes(std::size_t n) {
    need(n);
    const unsigned char* p = &buf_[pos_];
    pos_ += n;
    return p;
  }

  // As readCharSafe(): n bytes as a string; any embedded NUL gives n spaces.
  SEXP readCharSafe(std::size_t n) {
    if (n == 0) return R_BlankString;
    if (n > static_cast<std::size_t>(INT_MAX)) stop("conquestr: string too long (%d bytes)", n);
    const char* p = reinterpret_cast<const char*>(readBytes(n));
    if (std::memchr(p, 0, n) != nullptr) {
      std::string s(n, ' ');
      return Rf_mkCharLenCE(s.data(), static_cast<int>(n), CE_NATIVE);
    }
    return Rf_mkCharLenCE(p, static_cast<int>(n), CE_NATIVE);
  }

  // As readChar(con, 1): one byte; a NUL gives "".
  SEXP readChar1() {
    char c = static_cast<char>(*readBytes(1));
    if (c == 0) return R_BlankString;
    return Rf_mkCharLenCE(&c, 1, CE_NATIVE);
  }

  std::size_t pos() const { return pos_; }

private:
  void need(std::size_t n) const {
    if (n > buf_.size() - pos_) {
      stop(
        "conquestr: unexpected end of file (needed %d bytes at byte offset %d of %d)",
        n, pos_, buf_.size()
      );
    }
  }

  std::vector<unsigned char> buf_;
  std::size_t pos_;
};

CqReader& rd(SEXP x) {
  if (TYPEOF(x) != EXTPTRSXP || !Rf_inherits(x, "cqReader")) {
    stop("`myFile` must be a 'cqReader' (see `ConQuestSys()`), not a connection");
  }
  CqReader* p = static_cast<CqReader*>(R_ExternalPtrAddr(x));
  if (p == nullptr) stop("conquestr: invalid 'cqReader' (was it saved and reloaded?)");
  return *p;
}

// ---- helpers ---------------------------------------------------------------

int readCount(CqReader& r) {
  int n = r.readInt();
  if (n < 0) {
    stop(
      "conquestr: invalid count (%d) before byte offset %d; the file may be corrupt or unsupported",
      n, r.pos()
    );
  }
  return n;
}

void checkNonNeg(int n, const char* what) {
  if (n < 0 || n == NA_INTEGER) stop("conquestr: invalid value for `%s` (%d)", what, n);
}

LogicalVector lgl1(int v) {
  LogicalVector x(1);
  x[0] = v;
  return x;
}

LogicalMatrix naMatrix() { // matrix()
  LogicalMatrix m(1, 1);
  m[0] = NA_LOGICAL;
  return m;
}

CharacterVector mkNames(std::initializer_list<const char*> nms) {
  CharacterVector out(nms.size());
  R_xlen_t i = 0;
  for (const char* s : nms) out[i++] = s;
  return out;
}

// i-th (0-based) element of a list of scalars, or of an atomic vector, as int
int intAt(SEXP x, R_xlen_t i) {
  if (i >= Rf_xlength(x)) stop("subscript out of bounds");
  if (TYPEOF(x) == VECSXP) return as<int>(VECTOR_ELT(x, i));
  switch (TYPEOF(x)) {
  case INTSXP: return INTEGER(x)[i];
  case LGLSXP: return LOGICAL(x)[i];
  case REALSXP: return static_cast<int>(REAL(x)[i]);
  default: stop("conquestr: expected a list or vector of integers");
  }
  return NA_INTEGER; // not reached
}

// x$name for a list (exact match), R_NilValue when absent
SEXP eltByName(SEXP x, const char* name) {
  if (TYPEOF(x) != VECSXP) return R_NilValue;
  SEXP nms = Rf_getAttrib(x, R_NamesSymbol);
  if (Rf_isNull(nms)) return R_NilValue;
  for (R_xlen_t i = 0; i < Rf_xlength(x); ++i) {
    if (std::strcmp(CHAR(STRING_ELT(nms, i)), name) == 0) return VECTOR_ELT(x, i);
  }
  return R_NilValue;
}

template <typename F>
List readList(CqReader& r, F f) {
  int n = readCount(r);
  List out(n);
  for (int i = 0; i < n; ++i) out[i] = f(r);
  return out;
}

// ---- primitives -------------------------------------------------------------

double rdDouble(CqReader& r) { return r.readDouble(); }
int rdInteger(CqReader& r) { return r.readInt(); }
LogicalVector rdBoolean(CqReader& r) { return lgl1(r.readBoolByte()); }

CharacterVector rdString(CqReader& r) {
  int L = r.readInt();
  CharacterVector out(1); // ""
  if (L == 0) return out;
  if (L < 0) stop("conquestr: invalid string length (%d) before byte offset %d", L, r.pos());
  SET_STRING_ELT(out, 0, r.readCharSafe(static_cast<std::size_t>(L)));
  return out;
}

CharacterVector rdSign(CqReader& r) {
  CharacterVector out(1);
  SET_STRING_ELT(out, 0, r.readChar1());
  return out;
}

// a string list as a character vector (as unlist(ReadStringList()))
CharacterVector rdStringVec(CqReader& r) {
  int n = readCount(r);
  CharacterVector out(n);
  for (int i = 0; i < n; ++i) {
    int L = r.readInt();
    if (L < 0) stop("conquestr: invalid string length (%d) before byte offset %d", L, r.pos());
    SET_STRING_ELT(out, i, r.readCharSafe(static_cast<std::size_t>(L)));
  }
  return out;
}

List rdDoubleList(CqReader& r) { return readList(r, rdDouble); }
List rdIntegerList(CqReader& r) { return readList(r, rdInteger); }
List rdIntegerListList(CqReader& r) { return readList(r, rdIntegerList); }
List rdBooleanList(CqReader& r) { return readList(r, rdBoolean); }
List rdStringList(CqReader& r) { return readList(r, rdString); }

// ---- matrices and bit sets --------------------------------------------------

IntegerMatrix rdIntArray2(CqReader& r) {
  int nr = r.readInt();
  int nc = r.readInt();
  checkNonNeg(nr, "NRows");
  checkNonNeg(nc, "NCols");
  IntegerMatrix A(nr, nc);
  for (int i = 0; i < nr; ++i)
    for (int j = 0; j < nc; ++j) A(i, j) = r.readInt();
  return A;
}

RObject rdMatrix(CqReader& r) {
  int rows = r.readInt();
  int cols = r.readInt();
  r.readBoolByte();          // Empty
  rdString(r);               // Title
  CharacterVector cl = rdStringVec(r);
  CharacterVector rl = rdStringVec(r);
  if (rows == 0 || cols == 0) return naMatrix();
  checkNonNeg(rows, "Rows");
  checkNonNeg(cols, "Columns");
  NumericMatrix A(rows, cols);
  for (int i = 0; i < rows; ++i)
    for (int j = 0; j < cols; ++j) A(i, j) = r.readDouble();
  if (cl.size() > 0 || rl.size() > 0) {
    if (cl.size() > 0 && cl.size() != cols) stop("length of 'dimnames' [2] not equal to array extent");
    if (rl.size() > 0 && rl.size() != rows) stop("length of 'dimnames' [1] not equal to array extent");
    List dn(2);
    if (rl.size() > 0) dn[0] = rl;
    if (cl.size() > 0) dn[1] = cl;
    A.attr("dimnames") = dn;
  }
  return A;
}

List rdMatrixList(CqReader& r) { return readList(r, rdMatrix); }

List rdMatrixVars(CqReader& r) {
  int n = readCount(r);
  if (n == 0) return List(0);
  List vals(n);
  CharacterVector nms(n);
  int k = 0;
  for (int i = 0; i < n; ++i) {
    CharacterVector nm = rdString(r);
    RObject m = rdMatrix(r);
    // m[[name]] <- value: replaces the first element with that name ("" never matches)
    const char* s = CHAR(STRING_ELT(nm, 0));
    int idx = -1;
    if (s[0] != '\0') {
      for (int j = 0; j < k; ++j) {
        if (std::strcmp(CHAR(STRING_ELT(nms, j)), s) == 0) { idx = j; break; }
      }
    }
    if (idx >= 0) {
      vals[idx] = m;
    } else {
      vals[k] = m;
      SET_STRING_ELT(nms, k, STRING_ELT(nm, 0));
      ++k;
    }
  }
  List out(k);
  CharacterVector outNames(k);
  for (int j = 0; j < k; ++j) {
    out[j] = vals[j];
    SET_STRING_ELT(outNames, j, STRING_ELT(nms, j));
  }
  out.attr("names") = outNames;
  return out;
}

List rdBitSet(CqReader& r) {
  int items = r.readInt();
  int cols = r.readInt();
  int size = r.readInt();
  RObject s = naMatrix();
  if (size > 0) {
    const unsigned char* bytes = r.readBytes(static_cast<std::size_t>(size));
    checkNonNeg(items, "Items");
    checkNonNeg(cols, "Columns");
    LogicalMatrix m(items, cols);
    const long long total = static_cast<long long>(items) * cols;
    const long long nbits = 8LL * size;
    for (long long k = 0; k < total; ++k) { // filled by row; bits are LSB first
      m(static_cast<int>(k / cols), static_cast<int>(k % cols)) =
        k < nbits ? ((bytes[k / 8] >> (k % 8)) & 1) : NA_LOGICAL;
    }
    s = m;
  }
  return List::create(_["String"] = s, _["Items"] = items, _["Columns"] = cols, _["Size"] = size);
}

// ---- records ----------------------------------------------------------------

List rdTerm(CqReader& r) {
  int varNo = r.readInt();
  List valueList = rdStringList(r);
  int whichCategorise = r.readInt();
  int nDummies = r.readInt();
  LogicalVector hasDummies = rdBoolean(r);
  return List::create(
    _["VarNo"] = varNo, _["ValueList"] = valueList, _["WhichCategorise"] = whichCategorise,
    _["NDummies"] = nDummies, _["HasDummies"] = hasDummies
  );
}

List rdTerms(CqReader& r, int cqsVersion) {
  int n = readCount(r);
  int np = readCount(r);
  List variableNumber(n), variableType(n), paramNumber(np), paramType(np);
  for (int i = 0; i < n; ++i) variableNumber[i] = r.readInt();
  for (int i = 0; i < n; ++i) variableType[i] = r.readInt();
  for (int i = 0; i < np; ++i) paramNumber[i] = r.readInt();
  for (int i = 0; i < np; ++i) paramType[i] = r.readInt();
  CharacterVector sign = rdSign(r);
  if (cqsVersion >= 26) {
    int constraint = r.readInt();
    CharacterVector label = rdString(r);
    return List::create(
      _["VariableNumber"] = variableNumber, _["VariableType"] = variableType,
      _["ParamNumber"] = paramNumber, _["ParamType"] = paramType, _["Sign"] = sign,
      _["Constraint"] = constraint, _["Label"] = label
    );
  }
  CharacterVector label = rdString(r);
  return List::create(
    _["VariableNumber"] = variableNumber, _["VariableType"] = variableType,
    _["ParamNumber"] = paramNumber, _["ParamType"] = paramType, _["Sign"] = sign,
    _["Label"] = label
  );
}

List rdTermsList(CqReader& r, int cqsVersion) {
  int n = readCount(r);
  List out(n);
  for (int i = 0; i < n; ++i) out[i] = rdTerms(r, cqsVersion);
  return out;
}

List rdErrors(CqReader& r) {
  LogicalVector init = rdBoolean(r);
  RObject errormatrix = rdMatrix(r);
  int errorstype = r.readInt();
  int nXsi = r.readInt();
  int nTau = r.readInt();
  int nBeta = r.readInt();
  int nSigma = r.readInt();
  int nRandomGroup = r.readInt();
  List xsiIndex = rdIntegerList(r);
  List tauIndex = rdIntegerList(r);
  IntegerMatrix betaIndex = rdIntArray2(r);
  IntegerMatrix sigmaIndex = rdIntArray2(r);
  IntegerMatrix randomGroupIndex = rdIntArray2(r);
  return List::create(
    init, errormatrix, errorstype, nXsi, nTau, nBeta, nSigma, nRandomGroup,
    xsiIndex, tauIndex, betaIndex, sigmaIndex, randomGroupIndex
  );
}

List rdImplicitVar(CqReader& r) {
  int L = readCount(r);
  List name(L), levels(L);
  for (int i = 0; i < L; ++i) name[i] = rdString(r);
  for (int i = 0; i < L; ++i) levels[i] = r.readInt();
  return List::create(_["Name"] = name, _["Levels"] = levels);
}

List rdVarList(CqReader& r) {
  List xv = rdIntegerList(r);
  List iv = rdIntegerList(r);
  return List::create(_["XV"] = xv, _["IV"] = iv);
}

List rdLookUp(CqReader& r) {
  int varNumber = r.readInt();
  List value = rdStringList(r);
  return List::create(_["VarNumber"] = varNumber, _["Value"] = value);
}

List rdBandDefine(CqReader& r) {
  int dimension = r.readInt();
  double upper = r.readDouble();
  double lower = r.readDouble();
  CharacterVector label = rdString(r);
  return List::create(
    _["Dimension"] = dimension, _["Upper Bound"] = upper, _["Lower Bound"] = lower,
    _["Band Label"] = label
  );
}

List rdAnchor(CqReader& r) {
  int type = r.readInt();
  int i1 = r.readInt();
  int i2 = r.readInt();
  double value = r.readDouble();
  return List::create(_["Type"] = type, _["I1"] = i1, _["I2"] = i2, _["Value"] = value);
}

List rdVariable(CqReader& r) {
  CharacterVector name = rdString(r);
  int n = readCount(r);
  List begin(n), end(n);
  for (int i = 0; i < n; ++i) begin[i] = r.readInt();
  for (int i = 0; i < n; ++i) end[i] = r.readInt();
  for (int i = 0; i < n; ++i) r.readInt(); // Record (not returned)
  int level = r.readInt();
  List missingList = rdStringList(r);
  List missingMatchMethod = rdIntegerList(r);
  List dropKeepList = rdStringList(r);
  List dropKeepMatchMethod = rdIntegerList(r);
  int dropKeepType = r.readInt();
  LogicalVector categorical = rdBoolean(r);
  List c = rdStringList(r);
  return List::create(
    _["Name"] = name, _["Begin"] = begin, _["End"] = end, _["Level"] = level,
    _["MissingList"] = missingList, _["MissingMatchMethod"] = missingMatchMethod,
    _["DropKeepList"] = dropKeepList, _["DropKeepMatchMethod"] = dropKeepMatchMethod,
    _["DropKeepType"] = dropKeepType, _["Categorical"] = categorical, _["c"] = c
  );
}

List rdResponse(CqReader& r) {
  int n = readCount(r);
  List col(n), rec(n);
  for (int i = 0; i < n; ++i) col[i] = r.readInt();
  for (int i = 0; i < n; ++i) rec[i] = r.readInt();
  int width = r.readInt();
  return List::create(_["Col"] = col, _["Rec"] = rec, _["Width"] = width);
}

List rdKey(CqReader& r) {
  List key = rdStringList(r);
  CharacterVector score = rdString(r);
  return List::create(_["Key"] = key, _["Score"] = score);
}

List rdLabel(CqReader& r) {
  int varNum = r.readInt();
  int varType = r.readInt();
  int n = readCount(r);
  List code(n), label(n);
  for (int i = 0; i < n; ++i) code[i] = rdString(r);
  for (int i = 0; i < n; ++i) label[i] = rdString(r);
  return List::create(_["VarNum"] = varNum, _["VarType"] = varType, _["Code"] = code, _["Label"] = label);
}

List rdVarInfo(CqReader& r) {
  int varNumber = r.readInt();
  int varType = r.readInt();
  return List::create(_["VarNumber"] = varNumber, _["VarType"] = varType);
}

List rdCodeList(CqReader& r) {
  List varInfo = rdVarInfo(r);
  List s = rdStringList(r);
  return List::create(_["VarInfo"] = varInfo, _["S"] = s);
}

List rdIRecode(CqReader& r) {
  List before = rdStringList(r);
  List afterList = readList(r, rdStringList);
  List cList = readList(r, rdCodeList);
  return List::create(_["Before"] = before, _["AfterList"] = afterList, _["CList"] = cList);
}

List rdParameters(CqReader& r) {
  List levels = rdIntegerList(r);
  int step = r.readInt();
  CharacterVector sign = rdSign(r);
  return List::create(_["Levels"] = levels, _["Step"] = step, _["Sign"] = sign);
}

List rdCategoriseLegacy(CqReader& r) {
  int type = r.readInt();
  int varnum = r.readInt();
  List valueList = rdStringList(r);
  List matchType = rdIntegerList(r);
  return List::create(_["Type"] = type, _["Varnum"] = varnum, _["ValueList"] = valueList, _["MatchType"] = matchType);
}

List rdCategorise(CqReader& r) {
  int type = r.readInt();
  int varnum = r.readInt();
  List valueList = rdStringList(r);
  List matchType = rdIntegerList(r);
  List dummyVectors = rdMatrixVars(r);
  return List::create(
    _["Type"] = type, _["Varnum"] = varnum, _["ValueList"] = valueList,
    _["MatchType"] = matchType, _["DummyVectors"] = dummyVectors
  );
}

List rdCategoriseList(CqReader& r) { return readList(r, rdCategorise); }

List rdRegressors(CqReader& r) {
  CharacterVector gModelText = rdString(r);
  List regressionTerms = readList(r, rdTerm);
  int nparams = r.readInt();
  int nRegVars = r.readInt();
  rdCategoriseList(r); // CategoriseList (not returned)
  LogicalVector randomExists = rdBoolean(r);
  CharacterVector randomGroup = rdString(r);
  List nRegByD = rdIntegerList(r);
  List constraints = rdMatrixList(r);
  List regLookUp = rdMatrixList(r);
  return List::create(
    _["gModelText"] = gModelText, _["RegressionTerms"] = regressionTerms, _["Nparams"] = nparams,
    _["NRegVars"] = nRegVars, _["RandomExists"] = randomExists, _["RandomGroup"] = randomGroup,
    _["NRegByD"] = nRegByD, _["Constraints"] = constraints, _["RegLookUp"] = regLookUp
  );
}

List rdFit(CqReader& r) {
  double v[9];
  for (int i = 0; i < 9; ++i) v[i] = r.readDouble();
  LogicalVector failed = rdBoolean(r);
  return List::create(
    _["UnWeightedMNSQ"] = v[0], _["UnWeightedtfit"] = v[1], _["WeightedCW2"] = v[2],
    _["WeightedMNSQ"] = v[3], _["Weightedtfit"] = v[4], _["WeightedNumerator"] = v[5],
    _["WeightedDenominator"] = v[6], _["UnWeightedSE"] = v[7], _["WeightedSE"] = v[8],
    _["Failed"] = failed
  );
}

List rdFitList(CqReader& r) {
  int n = readCount(r);
  List names(n), value(n);
  for (int i = 0; i < n; ++i) {
    names[i] = rdString(r);
    value[i] = rdFit(r);
  }
  return List::create(_["Names"] = names, _["Value"] = value);
}

List rdRegression(CqReader& r) {
  int varnum = r.readInt();
  List valueList = rdStringList(r);
  return List::create(_["Varnum"] = varnum, _["ValueList"] = valueList);
}

List rdRegressionList(CqReader& r) {
  List regVars = readList(r, rdRegression);
  CharacterVector randGroupName = rdString(r);
  LogicalVector randGroupExists = rdBoolean(r);
  return List::create(
    _["RegVars"] = regVars, _["RandGroupName"] = randGroupName, _["RandGroupExists"] = randGroupExists
  );
}

List rdItemSet(CqReader& r) {
  CharacterVector name = rdString(r);
  List valueList = rdIntegerList(r);
  return List::create(_["Name"] = name, _["ValueList"] = valueList);
}

List rdHistory(CqReader& r) {
  int n = readCount(r);
  List runNo(n), iter(n), likelihood(n), beta(n), withinVariance(n), xsi(n), tau(n),
    ranTermVariance(n), betweenVariance(n);
  for (int i = 0; i < n; ++i) {
    runNo[i] = r.readInt();
    iter[i] = r.readInt();
    likelihood[i] = r.readDouble();
    beta[i] = rdMatrix(r);
    withinVariance[i] = rdMatrix(r);
    xsi[i] = rdMatrix(r);
    tau[i] = rdMatrix(r);
    ranTermVariance[i] = rdMatrix(r);
    betweenVariance[i] = rdMatrix(r);
  }
  return List::create(
    _["RunNo"] = runNo, _["Iter"] = iter, _["Likelihood"] = likelihood, _["Beta"] = beta,
    _["Variance"] = withinVariance, _["Xsi"] = xsi, _["Tau"] = tau,
    _["RanTermVariance"] = ranTermVariance, _["BetweenVariance"] = betweenVariance
  );
}

// ---- per-case data (the hot loops) -----------------------------------------

void fillVec(CqReader& r, NumericVector& v) {
  for (R_xlen_t i = 0; i < v.size(); ++i) v[i] = r.readDouble();
}

void fillMat(CqReader& r, NumericMatrix& m) { // row-major in the file
  for (int i = 0; i < m.nrow(); ++i)
    for (int j = 0; j < m.ncol(); ++j) m(i, j) = r.readDouble();
}

List rdEstimatesRecord(CqReader& r, int D, int NP, int n, int cqsVersion, const CharacterVector& nms) {
  checkNonNeg(D, "Dimensions");
  checkNonNeg(NP, "NPlausibles");
  NumericVector interimEap(D), interimEapErr(D), eap(D), wle(D), jml(D), scores(D), maxscores(D);
  NumericMatrix eapErr(D, D), wleErr(D, D), pvs(D, NP), jmlErr(D, D);
  fillVec(r, interimEap);
  fillVec(r, interimEapErr);
  fillVec(r, eap);
  fillMat(r, eapErr);
  fillVec(r, wle);
  fillMat(r, wleErr);
  fillMat(r, pvs);
  fillVec(r, jml);
  fillMat(r, jmlErr);
  fillVec(r, scores);
  fillVec(r, maxscores);
  RObject fit;
  if (cqsVersion >= 30) {
    NumericMatrix f(4, 1);
    fillMat(r, f);
    fit = f;
  } else {
    fit = wrap(r.readDouble());
  }
  double weight = r.readDouble();
  List out = List::create(
    n, interimEap, interimEapErr, eap, eapErr, wle, wleErr, pvs, jml, jmlErr,
    scores, maxscores, fit, weight
  );
  out.attr("names") = nms;
  return out;
}

CharacterVector estimatesNames() {
  return mkNames({
    "pid", "interim_eap", "interim_eaperr", "eap", "eaperr", "wle",
    "wleerr", "pvs", "jml", "jmlerr", "scores", "maxscores", "fit", "weight"
  });
}

List rdAllCaseEstimates(CqReader& r, int D, int N, int cqsVersion) {
  int chainLen = r.readInt();
  checkNonNeg(N, "N");
  CharacterVector nms = estimatesNames();
  List out(N);
  for (int i = 0; i < N; ++i) out[i] = rdEstimatesRecord(r, D, chainLen, i + 1, cqsVersion, nms);
  return out;
}

List rdDataRecord(CqReader& r, const CharacterVector& nms) {
  List out(5);
  out[0] = static_cast<double>(r.readInt()) + 1.0; // Pid (0-based in ConQuest)
  for (int k = 1; k < 5; ++k) out[k] = r.readInt();
  out.attr("names") = nms;
  return out;
}

CharacterVector dataRecordNames() { return mkNames({"Pid", "Rsp", "Item", "PreKeyRsp", "RspFlag"}); }

List rdAllResponseData(CqReader& r, int N) {
  checkNonNeg(N, "N");
  CharacterVector nms = dataRecordNames();
  List out(N);
  for (int i = 0; i < N; ++i) out[i] = rdDataRecord(r, nms);
  return out;
}

List rdYOneCase(CqReader& r, int nReg, const CharacterVector& nms) {
  checkNonNeg(nReg, "NReg");
  double weight = r.readDouble();
  NumericVector y(nReg);
  fillVec(r, y);
  List out = List::create(weight, y);
  out.attr("names") = nms;
  return out;
}

List rdAllY(CqReader& r, int N, int nReg) {
  checkNonNeg(N, "N");
  CharacterVector nms = mkNames({"Weight", "Y"});
  List out(N);
  for (int i = 0; i < N; ++i) out[i] = rdYOneCase(r, nReg, nms);
  return out;
}

// widths (End[[1]] - Begin[[1]] + 1) of the group variables
std::vector<std::size_t> groupWidths(SEXP groupVariables, SEXP allVariables) {
  SEXP xv = eltByName(groupVariables, "XV");
  R_xlen_t ng = Rf_xlength(xv);
  std::vector<std::size_t> w(ng);
  for (R_xlen_t k = 0; k < ng; ++k) {
    int which = intAt(xv, k);
    if (TYPEOF(allVariables) != VECSXP || which < 0 || which >= Rf_xlength(allVariables)) {
      stop("subscript out of bounds");
    }
    SEXP var = VECTOR_ELT(allVariables, which);
    SEXP end = eltByName(var, "End");
    SEXP begin = eltByName(var, "Begin");
    if (Rf_xlength(end) < 1 || Rf_xlength(begin) < 1) stop("subscript out of bounds");
    long long L = static_cast<long long>(intAt(end, 0)) - intAt(begin, 0) + 1;
    if (L < 0) stop("invalid 'n' argument");
    w[k] = static_cast<std::size_t>(L);
  }
  return w;
}

List rdGroupsOneCase(CqReader& r, const std::vector<std::size_t>& w, SEXP caseNum, const CharacterVector& nms) {
  r.readChar1(); // Dummy
  if (w.empty()) return List(0);
  CharacterVector gdata(w.size());
  for (std::size_t k = 0; k < w.size(); ++k) SET_STRING_ELT(gdata, k, r.readCharSafe(w[k]));
  List out = List::create(caseNum, gdata);
  out.attr("names") = nms;
  return out;
}

List rdAllGroupsData(CqReader& r, int N, SEXP groupVariables, SEXP allVariables) {
  checkNonNeg(N, "N");
  List out(N);
  if (N == 0) return out;
  std::vector<std::size_t> w = groupWidths(groupVariables, allVariables);
  CharacterVector nms = mkNames({"CaseNum", "GData"});
  for (int i = 0; i < N; ++i) {
    IntegerVector caseNum = IntegerVector::create(i + 1);
    out[i] = rdGroupsOneCase(r, w, caseNum, nms);
  }
  return out;
}

// ---- design matrices --------------------------------------------------------

List rdADesignMatrices(CqReader& r, int columns, int items, SEXP itemSteps) {
  if (columns == 0) return List(0);
  checkNonNeg(columns, "Columns");
  checkNonNeg(items, "Items");
  List out(items);
  for (int i = 0; i < items; ++i) {
    int steps = intAt(itemSteps, i);
    if (steps > 0) {
      NumericMatrix m(steps, columns);
      fillMat(r, m);
      out[i] = m;
    } else {
      out[i] = naMatrix();
    }
  }
  return out;
}

List rdStepMatrices(CqReader& r, int steps) {
  if (steps == 0) steps = 1;
  checkNonNeg(steps, "ItemSteps");
  List out(steps);
  for (int k = 0; k < steps; ++k) out[k] = rdMatrix(r);
  return out;
}

List rdBDesignMatrices(CqReader& r, SEXP itemSteps, int items) {
  checkNonNeg(items, "Items");
  List out(items);
  for (int i = 0; i < items; ++i) out[i] = rdStepMatrices(r, intAt(itemSteps, i));
  return out;
}

List rdCDesignMatrices(CqReader& r, int dimensions, SEXP itemSteps, int items) {
  checkNonNeg(dimensions, "Dimensions");
  checkNonNeg(items, "Items");
  List out(dimensions);
  for (int d = 0; d < dimensions; ++d) {
    List byItem(items);
    for (int i = 0; i < items; ++i) byItem[i] = rdStepMatrices(r, intAt(itemSteps, i));
    out[d] = byItem;
  }
  return out;
}

// ---- rout files (graphs) ----------------------------------------------------

List rdPoint(CqReader& r) {
  double x = r.readDouble();
  double y = r.readDouble();
  double z = r.readDouble();
  CharacterVector label = rdString(r);
  return List::create(_["x"] = x, _["y"] = y, _["z"] = z, _["Label"] = label);
}

List rdSeries(CqReader& r) {
  int sType = r.readInt();
  int pointCount = r.readInt();
  checkNonNeg(pointCount, "PointCount");
  List points(pointCount);
  for (int i = 0; i < pointCount; ++i) points[i] = rdPoint(r);
  double minX = r.readDouble();
  double maxX = r.readDouble();
  double minY = r.readDouble();
  double maxY = r.readDouble();
  CharacterVector name = rdString(r);
  LogicalVector drawSeries = rdBoolean(r);
  LogicalVector drawPoints = rdBoolean(r);
  LogicalVector joinPoints = rdBoolean(r);
  LogicalVector labelPoints = rdBoolean(r);
  int lineWidth = r.readInt();
  int pointColour = r.readInt();
  int lineColour = r.readInt();
  int pointStyle = r.readInt();
  int labelStyle = r.readInt();
  int lineStyle = r.readInt();
  return List::create(
    _["SType"] = sType, _["PointCount"] = pointCount, _["Points"] = points,
    _["MinX"] = minX, _["MaxX"] = maxX, _["MinY"] = minY, _["MaxY"] = maxY,
    _["Name"] = name, _["DrawSeries"] = drawSeries, _["DrawPoints"] = drawPoints,
    _["JoinPoints"] = joinPoints, _["LabelPoints"] = labelPoints,
    _["LineWidth"] = lineWidth, _["PointColour"] = pointColour, _["LineColour"] = lineColour,
    _["PointStyle"] = pointStyle, _["LabelStyle"] = labelStyle, _["LineStyle"] = lineStyle
  );
}

List rdGraph(CqReader& r) {
  int gType = r.readInt();
  int nSeries = r.readInt();
  checkNonNeg(nSeries, "NSeries");
  List series(nSeries);
  for (int i = 0; i < nSeries; ++i) series[i] = rdSeries(r);
  double minX = r.readDouble();
  double maxX = r.readDouble();
  double minY = r.readDouble();
  double maxY = r.readDouble();
  int pointColourIndex = r.readInt();
  int lineColourIndex = r.readInt();
  int pointStyleIndex = r.readInt();
  r.readInt(); // FreeTextCount (not returned)
  int L = r.readInt();
  CharacterVector graphTitleText = rdString(r);
  CharacterVector graphSubTitleText = rdString(r);
  CharacterVector xAxisLabelText = rdString(r);
  CharacterVector yAxisLabelText = rdString(r);
  CharacterVector difficultyLabelText = rdString(r);
  CharacterVector fitLabelText = rdString(r);
  double nStrings = static_cast<double>(L) - 6.0;
  if (nStrings < 0) stop("conquestr: invalid number of graph strings (%d)", L);
  List strings(static_cast<int>(nStrings));
  for (int i = 0; i < static_cast<int>(nStrings); ++i) strings[i] = rdPoint(r);
  // List::create() takes at most 20 arguments, so names are set separately
  List out = List::create(
    gType, nSeries, series, minX, maxX, minY, maxY, pointColourIndex, lineColourIndex,
    pointStyleIndex, graphTitleText, graphSubTitleText, xAxisLabelText, yAxisLabelText,
    difficultyLabelText, fitLabelText, nStrings, strings
  );
  out.attr("names") = mkNames({
    "GType", "NSeries", "Series", "MinX", "MaxX", "MinY", "MaxY", "PointColourIndex",
    "LineColourIndex", "PointStyleIndex", "GraphTitleText", "GraphSubTitleText",
    "xAxisLabelText", "yAxisLabelText", "DifficultyLabelText", "FitLabelText",
    "NStrings", "Strings"
  });
  return out;
}

// ---- misc -------------------------------------------------------------------

List rdRandomStructure(CqReader& r) {
  CharacterVector termName = rdString(r);
  int termNumber = r.readInt();
  List randomL = rdBooleanList(r);
  RObject randomV = rdMatrix(r);
  return List::create(
    _["termName"] = termName, _["termNumber"] = termNumber, _["randomL"] = randomL, _["randomV"] = randomV
  );
}

List rdGExportOptions(CqReader& r) {
  int fileFormat = r.readInt();
  int sort = r.readInt();
  int dataType = r.readInt();
  CharacterVector fileName = rdString(r);
  return List::create(
    _["File Format"] = fileFormat, _["Sort"] = sort, _["Data Type"] = dataType, _["File Name"] = fileName
  );
}

List rdNamedStringList(CqReader& r) {
  int n = readCount(r);
  CharacterVector names(n), data(n);
  for (int i = 0; i < n; ++i) {
    names[i] = rdString(r)[0];
    data[i] = rdString(r)[0];
  }
  return List::create(_["names"] = names, _["data"] = data);
}

} // namespace

// ---- exported to R (called by the wrappers in R/ReadConQuestLibrary.R) -------

// [[Rcpp::export(.cqReaderNew)]]
SEXP cqReaderNew(RawVector x) {
  XPtr<CqReader> p(new CqReader(x), true);
  p.attr("class") = "cqReader";
  return p;
}

// [[Rcpp::export(.cqReadDouble)]]
double cqReadDouble(SEXP myFile) { return rdDouble(rd(myFile)); }
// [[Rcpp::export(.cqReadInteger)]]
int cqReadInteger(SEXP myFile) { return rdInteger(rd(myFile)); }
// [[Rcpp::export(.cqReadBoolean)]]
LogicalVector cqReadBoolean(SEXP myFile) { return rdBoolean(rd(myFile)); }
// [[Rcpp::export(.cqReadString)]]
CharacterVector cqReadString(SEXP myFile) { return rdString(rd(myFile)); }
// [[Rcpp::export(.cqReadDoubleList)]]
List cqReadDoubleList(SEXP myFile) { return rdDoubleList(rd(myFile)); }
// [[Rcpp::export(.cqReadIntegerList)]]
List cqReadIntegerList(SEXP myFile) { return rdIntegerList(rd(myFile)); }
// [[Rcpp::export(.cqReadIntegerListList)]]
List cqReadIntegerListList(SEXP myFile) { return rdIntegerListList(rd(myFile)); }
// [[Rcpp::export(.cqReadBooleanList)]]
List cqReadBooleanList(SEXP myFile) { return rdBooleanList(rd(myFile)); }
// [[Rcpp::export(.cqReadStringList)]]
List cqReadStringList(SEXP myFile) { return rdStringList(rd(myFile)); }
// [[Rcpp::export(.cqReadNamedStringList)]]
List cqReadNamedStringList(SEXP myFile) { return rdNamedStringList(rd(myFile)); }
// [[Rcpp::export(.cqReadIntArray2)]]
IntegerMatrix cqReadIntArray2(SEXP myFile) { return rdIntArray2(rd(myFile)); }
// [[Rcpp::export(.cqReadTerm)]]
List cqReadTerm(SEXP myFile) { return rdTerm(rd(myFile)); }
// [[Rcpp::export(.cqReadBitSet)]]
List cqReadBitSet(SEXP myFile) { return rdBitSet(rd(myFile)); }
// [[Rcpp::export(.cqReadMatrix)]]
RObject cqReadMatrix(SEXP myFile) { return rdMatrix(rd(myFile)); }
// [[Rcpp::export(.cqReadMatrixList)]]
List cqReadMatrixList(SEXP myFile) { return rdMatrixList(rd(myFile)); }
// [[Rcpp::export(.cqReadErrors)]]
List cqReadErrors(SEXP myFile) { return rdErrors(rd(myFile)); }
// [[Rcpp::export(.cqReadRegressors)]]
List cqReadRegressors(SEXP myFile) { return rdRegressors(rd(myFile)); }
// [[Rcpp::export(.cqReadImplicitVar)]]
List cqReadImplicitVar(SEXP myFile) { return rdImplicitVar(rd(myFile)); }
// [[Rcpp::export(.cqReadVarList)]]
List cqReadVarList(SEXP myFile) { return rdVarList(rd(myFile)); }
// [[Rcpp::export(.cqReadLookUp)]]
List cqReadLookUp(SEXP myFile) { return rdLookUp(rd(myFile)); }
// [[Rcpp::export(.cqReadLookUpList)]]
List cqReadLookUpList(SEXP myFile) { return readList(rd(myFile), rdLookUp); }
// [[Rcpp::export(.cqReadBandDefine)]]
List cqReadBandDefine(SEXP myFile) { return rdBandDefine(rd(myFile)); }
// [[Rcpp::export(.cqReadBandDefinesList)]]
List cqReadBandDefinesList(SEXP myFile) { return readList(rd(myFile), rdBandDefine); }
// [[Rcpp::export(.cqReadAnchor)]]
List cqReadAnchor(SEXP myFile) { return rdAnchor(rd(myFile)); }
// [[Rcpp::export(.cqReadAnchorList)]]
List cqReadAnchorList(SEXP myFile) { return readList(rd(myFile), rdAnchor); }
// [[Rcpp::export(.cqReadVariable)]]
List cqReadVariable(SEXP myFile) { return rdVariable(rd(myFile)); }
// [[Rcpp::export(.cqReadVariableList)]]
List cqReadVariableList(SEXP myFile) { return readList(rd(myFile), rdVariable); }
// [[Rcpp::export(.cqReadResponse)]]
List cqReadResponse(SEXP myFile) { return rdResponse(rd(myFile)); }
// [[Rcpp::export(.cqReadResponseList)]]
List cqReadResponseList(SEXP myFile) { return readList(rd(myFile), rdResponse); }
// [[Rcpp::export(.cqReadKey)]]
List cqReadKey(SEXP myFile) { return rdKey(rd(myFile)); }
// [[Rcpp::export(.cqReadKeyList)]]
List cqReadKeyList(SEXP myFile) { return readList(rd(myFile), rdKey); }
// [[Rcpp::export(.cqReadLabel)]]
List cqReadLabel(SEXP myFile) { return rdLabel(rd(myFile)); }
// [[Rcpp::export(.cqReadLabelList)]]
List cqReadLabelList(SEXP myFile) { return readList(rd(myFile), rdLabel); }
// [[Rcpp::export(.cqReadTerms)]]
List cqReadTerms(SEXP myFile, int cqs_version) { return rdTerms(rd(myFile), cqs_version); }
// [[Rcpp::export(.cqReadTermsList)]]
List cqReadTermsList(SEXP myFile, int cqs_version) { return rdTermsList(rd(myFile), cqs_version); }
// [[Rcpp::export(.cqReadVarInfo)]]
List cqReadVarInfo(SEXP myFile) { return rdVarInfo(rd(myFile)); }
// [[Rcpp::export(.cqReadCodeList)]]
List cqReadCodeList(SEXP myFile) { return rdCodeList(rd(myFile)); }
// [[Rcpp::export(.cqReadIRecode)]]
List cqReadIRecode(SEXP myFile) { return rdIRecode(rd(myFile)); }
// [[Rcpp::export(.cqReadIRecodeList)]]
List cqReadIRecodeList(SEXP myFile) { return readList(rd(myFile), rdIRecode); }
// [[Rcpp::export(.cqReadParameters)]]
List cqReadParameters(SEXP myFile) { return rdParameters(rd(myFile)); }
// [[Rcpp::export(.cqReadParametersList)]]
List cqReadParametersList(SEXP myFile) { return readList(rd(myFile), rdParameters); }
// [[Rcpp::export(.cqReadCategoriseLegacy)]]
List cqReadCategoriseLegacy(SEXP myFile) { return rdCategoriseLegacy(rd(myFile)); }
// [[Rcpp::export(.cqReadCategorise)]]
List cqReadCategorise(SEXP myFile) { return rdCategorise(rd(myFile)); }
// [[Rcpp::export(.cqReadCategoriseListLegacy)]]
List cqReadCategoriseListLegacy(SEXP myFile) { return readList(rd(myFile), rdCategoriseLegacy); }
// [[Rcpp::export(.cqReadCategoriseList)]]
List cqReadCategoriseList(SEXP myFile) { return rdCategoriseList(rd(myFile)); }
// [[Rcpp::export(.cqReadFit)]]
List cqReadFit(SEXP myFile) { return rdFit(rd(myFile)); }
// [[Rcpp::export(.cqReadFitList)]]
List cqReadFitList(SEXP myFile) { return rdFitList(rd(myFile)); }
// [[Rcpp::export(.cqReadRegression)]]
List cqReadRegression(SEXP myFile) { return rdRegression(rd(myFile)); }
// [[Rcpp::export(.cqReadRegressionListLeg)]]
List cqReadRegressionListLeg(SEXP myFile) { return readList(rd(myFile), rdRegression); }
// [[Rcpp::export(.cqReadRegressionList)]]
List cqReadRegressionList(SEXP myFile) { return rdRegressionList(rd(myFile)); }
// [[Rcpp::export(.cqReadItemSet)]]
List cqReadItemSet(SEXP myFile) { return rdItemSet(rd(myFile)); }
// [[Rcpp::export(.cqReadItemSetList)]]
List cqReadItemSetList(SEXP myFile) { return readList(rd(myFile), rdItemSet); }
// [[Rcpp::export(.cqReadHistory)]]
List cqReadHistory(SEXP myFile) { return rdHistory(rd(myFile)); }
// [[Rcpp::export(.cqReadEstimatesRecord)]]
List cqReadEstimatesRecord(SEXP myFile, int Dimensions, int NPlausibles, int n, int cqs_version) {
  return rdEstimatesRecord(rd(myFile), Dimensions, NPlausibles, n, cqs_version, estimatesNames());
}
// [[Rcpp::export(.cqReadAllCaseEstimates)]]
List cqReadAllCaseEstimates(SEXP myFile, int Dimensions, int N, int cqs_version) {
  return rdAllCaseEstimates(rd(myFile), Dimensions, N, cqs_version);
}
// [[Rcpp::export(.cqReadDataRecord)]]
List cqReadDataRecord(SEXP myFile) { return rdDataRecord(rd(myFile), dataRecordNames()); }
// [[Rcpp::export(.cqReadAllResponseData)]]
List cqReadAllResponseData(SEXP myFile, int N) { return rdAllResponseData(rd(myFile), N); }
// [[Rcpp::export(.cqReadADesignMatrices)]]
List cqReadADesignMatrices(SEXP myFile, int Columns, int Items, SEXP ItemSteps) {
  return rdADesignMatrices(rd(myFile), Columns, Items, ItemSteps);
}
// [[Rcpp::export(.cqReadBDesignMatrices)]]
List cqReadBDesignMatrices(SEXP myFile, SEXP ItemSteps, int Items) {
  return rdBDesignMatrices(rd(myFile), ItemSteps, Items);
}
// [[Rcpp::export(.cqReadCDesignMatrices)]]
List cqReadCDesignMatrices(SEXP myFile, int Dimensions, SEXP ItemSteps, int Items) {
  return rdCDesignMatrices(rd(myFile), Dimensions, ItemSteps, Items);
}
// [[Rcpp::export(.cqReadYOneCase)]]
List cqReadYOneCase(SEXP myFile, int NReg) { return rdYOneCase(rd(myFile), NReg, mkNames({"Weight", "Y"})); }
// [[Rcpp::export(.cqReadAllY)]]
List cqReadAllY(SEXP myFile, int N, int NReg) { return rdAllY(rd(myFile), N, NReg); }
// [[Rcpp::export(.cqReadGroupsOneCase)]]
List cqReadGroupsOneCase(SEXP myFile, SEXP GroupVariables, SEXP AllVariables, SEXP CaseNum) {
  return rdGroupsOneCase(
    rd(myFile), groupWidths(GroupVariables, AllVariables), CaseNum, mkNames({"CaseNum", "GData"})
  );
}
// [[Rcpp::export(.cqReadAllGroupsData)]]
List cqReadAllGroupsData(SEXP myFile, int N, SEXP GroupVariables, SEXP AllVariables) {
  return rdAllGroupsData(rd(myFile), N, GroupVariables, AllVariables);
}
// [[Rcpp::export(.cqReadMatrixVars)]]
List cqReadMatrixVars(SEXP myFile) { return rdMatrixVars(rd(myFile)); }
// [[Rcpp::export(.cqReadPoint)]]
List cqReadPoint(SEXP myFile) { return rdPoint(rd(myFile)); }
// [[Rcpp::export(.cqReadSeries)]]
List cqReadSeries(SEXP myFile) { return rdSeries(rd(myFile)); }
// [[Rcpp::export(.cqReadGraph)]]
List cqReadGraph(SEXP myFile) { return rdGraph(rd(myFile)); }
// [[Rcpp::export(.cqReadRandomStructure)]]
List cqReadRandomStructure(SEXP myFile) { return rdRandomStructure(rd(myFile)); }
// [[Rcpp::export(.cqReadGExportOptions)]]
List cqReadGExportOptions(SEXP myFile) { return rdGExportOptions(rd(myFile)); }
// [[Rcpp::export(.cqReadGExportOptionsList)]]
List cqReadGExportOptionsList(SEXP myFile) { return readList(rd(myFile), rdGExportOptions); }
