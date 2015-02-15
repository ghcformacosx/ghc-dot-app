'use strict';
var defaultInfo = {
    bundlePath: '/Applications/GHC.app',
    bundleName: 'GHC',
    bundleVersion: '7.8.4',
    appName: 'ghc-7.8.4',
    inDownloadsDir: false,
    isBash: false,
    environment: {SHELL: "/bin/bash"},
    bashPaths: {bashrc: true,
                bash_profile: true,
                bash_login: true,
                profile: true},
    loginGHC: '',
    otherGHC: '',
    loginPATH: '/usr/bin',
    otherPATH: '/usr/bin',
    xcodePath: '/Applications/Xcode.app/Contents/Developer',

};
var context = window.info || defaultInfo;
var template = _.template(document.getElementById('page-template').innerHTML);
function renderTemplate(context) {
    document.body.innerHTML = template(context);
}
function codeBoxClicked(elem, e) {
    var sel = window.getSelection();
    if (sel.rangeCount > 0) {
        sel.removeAllRanges()
    }
    var range = document.createRange();
    range.selectNode(elem);
    sel.addRange(range);
    e.preventDefault();
    return false;
}
window.addEventListener('click', function globalClickHandler(e) {
    for (var t = e.target; t && t !== document; t = t.parentNode) {
        if (t.classList.contains('code-box')) {
            return codeBoxClicked(t, e);
        }
        if (t.nodeName === 'BUTTON' && t.dataset.url) {
            window.location = t.dataset.url;
        }
    }
});
window.addEventListener('load', function onLoad(e) {
    var el = document.documentElement,
        sw = el.scrollWidth,
        sh = el.scrollHeight;
    if (el.scrollWidth > el.clientWidth ||
        el.scrollHeight > el.clientHeight) {
        var dw = window.outerWidth - window.innerWidth,
            dh = window.outerHeight - window.innerHeight,
            marginRight = parseFloat(window.getComputedStyle(document.body).marginRight);
        window.resizeTo(
            Math.max(window.innerWidth, sw + marginRight) + dw,
            Math.max(window.innerHeight, sh) + dh);
    }
});
renderTemplate(context);
