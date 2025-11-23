// Manage a single WebSocket connection for the run log modal.
// This runs via Dash's assets pipeline.
(function(){
  function closeSocket() {
    if (window.runlogSocket) {
      try { window.runlogSocket.close(); } catch (e) {}
    }
    window.runlogSocket = null;
    window.runlogSocketPath = null;
  }

  window.handleRunlogSocket = function(logPath, isOpen) {
    var status = { connected: false, path: null };

    if (!logPath || !isOpen) {
      closeSocket();
      return status;
    }

    var target = document.getElementById('runlog-content');
    if (!target) {
      return window.dash_clientside ? window.dash_clientside.no_update : status;
    }

    if (window.runlogSocket && window.runlogSocketPath === logPath &&
        (window.runlogSocket.readyState === 0 || window.runlogSocket.readyState === 1)) {
      return { connected: true, path: logPath };
    }

    closeSocket();

    target.textContent = 'Connecting to log...';
    var protocol = window.location.protocol === 'https:' ? 'wss://' : 'ws://';
    var url = protocol + window.location.host + '/ws/runlog?path=' + encodeURIComponent(logPath);
    var sock = new WebSocket(url);
    window.runlogSocket = sock;
    window.runlogSocketPath = logPath;

    sock.onmessage = function(evt) {
      var el = document.getElementById('runlog-content');
      if (!el) { return; }
      if (evt.data === '__runlog_reset__') {
        el.textContent = '';
        return;
      }
      el.textContent += evt.data;
      el.scrollTop = el.scrollHeight;
    };

    sock.onclose = function(evt) {
      if (!isOpen) { return; }
      var el = document.getElementById('runlog-content');
      if (el && evt && evt.code !== 1000) {
        el.textContent += '\n[connection closed]';
      }
    };

    sock.onerror = function() {
      var el = document.getElementById('runlog-content');
      if (el) {
        el.textContent += '\n[WebSocket error]';
      }
    };

    return { connected: true, path: logPath };
  };

  window.addEventListener('beforeunload', closeSocket);
})();
