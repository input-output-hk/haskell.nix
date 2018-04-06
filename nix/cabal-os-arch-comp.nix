# Predicates used in conditional
# cabal expressions.
{
  compiler = {
    # Haskell compilers
    isGhc = false;
    isGhcjs = false;
    isNhc = false;
    isYhc = false;
    isHugs = false;
    isHbc = false;
    isHelium = false;
    isJhc = false;
    isLhc = false;
    isUhc = false;
    isEta = false;
  };
  os = {
    # OSs
    isLinux     = false;
    isWindows   = false;
    isOSX       = false;
    isFreeBSD   = false;
    isOpenBSD   = false;
    isNetBSD    = false;
    isDragonFly = false;
    isSolaris   = false;
    isAIX       = false;
    isHPUX      = false;
    isIRIX      = false;
    isHaLVM     = false;
    isHurd      = false;
    isIOS       = false;
    isAndroid   = false;
    isGhcjs     = false;
  };
  arch = {
    # Archs
    isI386   = false;
    isX86_64 = false;
    isPPC    = false;
    isPPC64  = false;
    isSparc  = false;
    isArm    = false;
    isAArch64= false;
    isMips   = false;
    isSH     = false;
    isIA64   = false;
    isS390   = false;
    isAlpha  = false;
    isHppa   = false;
    isRs6000 = false;
    isM68k   = false;
    isVax    = false;
    isJavaScript = false;
  };
}
