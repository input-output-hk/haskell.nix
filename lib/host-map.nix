stdenv: with stdenv.hostPlatform; {
  os = if isLinux   then "Linux"   else
       if isWindows then "Windows" else
       if isDarwin     then "Osx"     else
       if isFreeBSD then "Freebsd" else
       if isOpenBSD then "Openbsd" else
       if isNetBSD  then "Netbsd"  else
       if isiOS     then "Ios"     else
       if isAndroid then "Android" else
       if isGhcjs   then "Ghcjs"   else
       throw "Unknown OS";
  arch = if isx86     && is32bit then "I386"    else
         if isx86     && is64bit then "X86_64"  else
         if isPowerPC && is32bit then "PPC"     else
         if isPowerPC && is64bit then "PPC64"   else
         if isAarch32            then "Arm"     else
         if isAarch64            then "Aarch64" else
         if isMips               then "Mips"    else
         if isJavaScript         then "JavaScript" else
         throw "Unknown Arch";
}
