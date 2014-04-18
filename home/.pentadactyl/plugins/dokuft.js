(function() {
 var _savedCreateTempFile = dactyl.modules.io.createTempFile;
  
  var wrappedCreateTempFile = function(ext, label){
    console.log('createTempFile: ' + ext + ', ' + label);
    if(buffer.win.location.href.indexOf('dokuwiki') > 0) {
      ext = 'dokuwiki.' + ext;
    }
    return _savedCreateTempFile.call(null, ext, label);
  };

  dactyl.modules.io.createTempFile = wrappedCreateTempFile;
})();
