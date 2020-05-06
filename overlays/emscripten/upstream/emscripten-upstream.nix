{ emscriptenVersion, stdenv, fetchFromGitHub, cmake, python, gtest, ... }:

let
  gcc = if stdenv.cc.isGNU then stdenv.cc.cc else stdenv.cc.cc.gcc;
in
stdenv.mkDerivation rec {
  name = "emscripten-upstream";

  src = (fetchFromGitHub {
    owner = "llvm";
    repo = "llvm-project";
    rev = "5f7ea85e789d5b5f3f463e538a28c040e373620b";
    sha256 = "1kik1zdl1igkdri9zkdri7jwjjfjwhzniaw2a57lr80ajhs60kr7";
  });

  nativeBuildInputs = [ cmake python gtest ];
  setSourceRoot = ''
    sourceRoot=$(echo */llvm)
  '';
  cmakeFlags = [
    "-DCMAKE_CXX_FLAGS=-Wno-nonportable-include-path"
    "-DLLVM_ENABLE_LIBXML2=OFF"
    "-DLLVM_INCLUDE_EXAMPLES=OFF"
    "-DCOMPILER_RT_BUILD_XRAY=OFF"
    "-DCOMPILER_RT_INCLUDE_TESTS=OFF"
    "-DCOMPILER_RT_ENABLE_IOS=OFF"
    "-DLLVM_BUILD_LLVM_DYLIB=ON"
    "-DLLVM_LINK_LLVM_DYLIB=ON"
    "-DLLVM_TOOL_LTO_BUILD=OFF"
    "-DLLVM_INSTALL_TOOLCHAIN_ONLY=ON"
    "-DLLVM_ENABLE_ASSERTIONS=ON"
    "-DLLVM_TARGETS_TO_BUILD=X86;WebAssembly"
    "-DLLVM_ENABLE_PROJECTS=lld;clang"
    "-DLLVM_ENABLE_TERMINFO=0"

    "-DCMAKE_BUILD_TYPE=Release"
    "-DLLVM_INCLUDE_EXAMPLES=OFF"
    "-DLLVM_INCLUDE_TESTS=ON"
    #"-DLLVM_CONFIG=${llvm}/bin/llvm-config"
    "-DLLVM_BUILD_TESTS=ON"
    "-DCLANG_INCLUDE_TESTS=OFF"
  ] ++ (stdenv.lib.optional stdenv.isLinux
    # necessary for clang to find crtend.o
    "-DGCC_INSTALL_PREFIX=${gcc}"
  );
  enableParallelBuilding = true;

  passthru = {
    isClang = true;
    inherit gcc;
  };

  meta = with stdenv.lib; {
    homepage = "http://github.com/llvm/llvm-project";
    description = "LLVM";
    platforms = platforms.all;
    maintainers = with maintainers; [  ];
    license = stdenv.lib.licenses.ncsa;
  };
}
