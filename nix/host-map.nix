stdenv: with stdenv.hostPlatform; {
  os = if isLinux   then "Linux"   else
       if isWindows then "Windows" else
       if isMacOS   then "OSX"     else
       if isFreeBSD then "FreeBSD" else
       if isOpenBSD then "OpenBSD" else
       if isNetBSD  then "NetBSD"  else
       if isHurd    then "Hurd"    else
       if isiOS     then "IOS"     else
       if isAndroid then "Android" else
       throw "Unknown OS";
  arch = if isx86     && is32bit then "I386"    else
         if isx86     && is64bit then "X86_64"  else
         if isPowerPC && is32bit then "PPC"     else
         if isPowerPC && is64bit then "PPC64"   else
         if isArm     && is32bit then "Arm"     else
         if isArm     && is64bit then "Aarch64" else
         if isMips               then "Mips"    else
         throw "Unknown Arch";
}
