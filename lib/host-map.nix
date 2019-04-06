stdenv:
# set a few default values so we don't depend on what's exported from
# hostPlatform from whatever nixpkgs set we are working against.
with { isLinux = false; isWindows = false; isDarwin = false; isFreeBSD = false;
       isOpenBSD = false; isNetBSD = false; isiOS = false; isAndroid = false;
       isGhcjs = false; isAsterius = false; isHurd = false;
       isx86 = false; isPowerPC = false; isAarch32 = false; isAarch64 = false;
       isMips = false; isWasm = false; isJavaScript = false;
       is32bit = false; is64bit = false; };
with stdenv.hostPlatform; {
  os = if isLinux   then "Linux"   else
       if isWindows then "Windows" else
       if isDarwin  then "Osx"     else
       if isFreeBSD then "Freebsd" else
       if isOpenBSD then "Openbsd" else
       if isNetBSD  then "Netbsd"  else
       if isHurd    then "Hurd"    else
       if isiOS     then "Ios"     else
       if isAndroid then "Android" else
       if isGhcjs   then "Ghcjs"   else
       if isAsterius then "Asterius" else
       throw "Unknown OS";
  arch = if isx86     && is32bit then "I386"    else
         if isx86     && is64bit then "X86_64"  else
         if isPowerPC && is32bit then "PPC"     else
         if isPowerPC && is64bit then "PPC64"   else
         if isAarch32            then "Arm"     else
         if isAarch64            then "Aarch64" else
         if isMips               then "Mips"    else
         if isWasm    && is32bit then "Wasm32"  else
         if isWasm    && is64bit then "Wasm64"  else
         if isJavaScript         then "JavaScript" else
         throw "Unknown Arch";
}
