final: _prev: {
    emsdk = final.linkFarm "emsdk" [
      { name = "upstream/bin"; path = final.clang + "/bin"; }
      { name = "upstream/emscripten"; path = final.emscripten + "/bin"; }
      { name = "share"; path = final.emscripten + "/share"; }
    ];
}
