{ emscriptenVersion, stdenv, lib, fetchFromGitHub, emscriptenfastcomp, python, nodejs, closurecompiler
, jre, binaryen, enableWasm ? true ,  cmake, emscriptenBackend ? emscriptenfastcomp
}:

let
  rev = emscriptenVersion;
  appdir = "share/emscripten";
  binaryenVersioned = binaryen;
in

stdenv.mkDerivation {
  name = "emscripten-${rev}";

  src = fetchFromGitHub {
    owner = "emscripten-core";
    repo = "emscripten";
    sha256 = "10f2mlwh4s6v3rqghb7zdmv4ry0a4si915x4v17vp4nv7swkkwhn";
    rev = "fc6a4bb97b421b6dcf5806d0dc42ab8ddd4da026";
  };

  buildInputs = [ nodejs cmake python ];

  buildCommand = ''
    mkdir -p $out/${appdir}
    cp -r $src/* $out/${appdir}
    chmod -R +w $out/${appdir}
    grep -rl '^#!/usr.*python' $out/${appdir} | xargs sed -i -s 's@^#!/usr.*python.*@#!${python}/bin/python@'
    sed -i -e "s,EM_CONFIG = '~/.emscripten',EM_CONFIG = '$out/${appdir}/config'," $out/${appdir}/tools/shared.py
    sed -i -e 's,^.*did not see a source tree above the LLVM.*$,      return True,' $out/${appdir}/tools/shared.py
    sed -i -e 's,def check_sanity(force=False):,def check_sanity(force=False):\n  return,' $out/${appdir}/tools/shared.py
    # fixes cmake support
    sed -i -e "s/print \('emcc (Emscript.*\)/sys.stderr.write(\1); sys.stderr.flush()/g" $out/${appdir}/emcc.py
    mkdir $out/bin
    ln -s $out/${appdir}/{em++,em-config,emar,emar.py,embuilder.py,emcc,emcc.py,emcmake,emconfigure,emconfigure.py,emlink.py,emmake,emmake.py,emranlib,emranlib.py,emrun,emscons} $out/bin

    echo "EMSCRIPTEN_ROOT = '$out/${appdir}'" > $out/${appdir}/config
    echo "LLVM_ROOT = '${emscriptenBackend}/bin'" >> $out/${appdir}/config
    echo "PYTHON = '${python}/bin/python'" >> $out/${appdir}/config
    echo "NODE_JS = '${nodejs}/bin/node'" >> $out/${appdir}/config
    echo "JS_ENGINES = [NODE_JS]" >> $out/${appdir}/config
    echo "COMPILER_ENGINE = NODE_JS" >> $out/${appdir}/config
    echo "CLOSURE_COMPILER = '${closurecompiler}/share/java/closure-compiler-v${closurecompiler.version}.jar'" >> $out/${appdir}/config
    echo "JAVA = '${jre}/bin/java'" >> $out/${appdir}/config
    # to make the test(s) below work
    echo "SPIDERMONKEY_ENGINE = []" >> $out/${appdir}/config
    echo "EMCC_FAST_COMPILER = 0" >> $out/${appdir}/config
  ''
  + lib.optionalString enableWasm ''
    echo "BINARYEN_ROOT = '${binaryenVersioned}'" >> $out/share/emscripten/config
  ''
  +
  ''
    echo "--------------- running test -----------------"
    # quick hack to get the test working
    HOME=$TMPDIR
    cp $out/${appdir}/config $HOME/.emscripten
    export PATH=$PATH:$out/bin

    #export EMCC_DEBUG=2
    ${python}/bin/python $src/tests/runner.py test_hello_world
    echo "--------------- /running test -----------------"
  '';

  meta = with lib; {
    homepage = "https://github.com/emscripten-core/emscripten";
    description = "An LLVM-to-JavaScript Compiler";
    platforms = platforms.all;
    maintainers = with maintainers; [ qknight matthewbauer ];
    license = licenses.ncsa;
  };
}
