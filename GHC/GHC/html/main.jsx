'use strict';
var defaultInfo =
  { bundleName: 'GHC'
  , bundleVersion: '7.8.4'
  , appName: 'ghc-7.8.4'
  , bundlePath: '/Applications/GHC.app'
  , environment: {SHELL: "/bin/bash"}
  , paths:
    { shell: 'bash'
    , loginGHC: ''
    , otherGHC: ''
    , loginPATH: '/usr/bin'
    , otherPATH: '/usr/bin'
    , xcode: '/Applications/Xcode.app/Contents/Developer'
    }
  , profiles:
    { bashrc: true
    , bash_profile: true
    , bash_login: true
    , profile: true
    , zshenv: false
    }
  , inDownloadsDir: false
  };
var info = window.info || defaultInfo;

var CodeBox = React.createClass(
  { propTypes:
    { bundleVersion: React.PropTypes.string.isRequired
    , bundlePath: React.PropTypes.string.isRequired
    }
  , handleClick(e) {
    e.preventDefault();
    var sel = window.getSelection();
    if (sel.rangeCount > 0) {
      sel.removeAllRanges()
    }
    var range = document.createRange();
    range.selectNode(this.refs.code.getDOMNode());
    sel.addRange(range); }
  , render() {
    var {bundleVersion, bundlePath} = this.props;
    var code =
      [ `# Add GHC ${bundleVersion} to the PATH, via https://ghcformacosx.github.io/`
      , `export GHC_DOT_APP="${bundlePath}"`
      , 'if [ -d "$GHC_DOT_APP" ]; then'
      , '  export PATH="${HOME}/.cabal/bin:${GHC_DOT_APP}/Contents/bin:${PATH}"'
      , 'fi'
      , ''
      ].join('\n');
    return (
      <div className="code-box" onClick={this.handleClick}>
        <code ref="code">{code}</code>
      </div>); }
  });

var Invocation = React.createClass(
  { propTypes: { shell: React.PropTypes.string }
  , render() {
    switch (this.props.shell) {
      case 'bash':
        return (
          <p>
            You can read more about the bash startup sequence in the INVOCATION
            section of bash documentation accessible with <a href="ghcdotapp://man/bash">'man bash'</a>.
          </p>);
      case 'zsh':
        return (
          <p>
            You can read more about the zsh startup sequence in the STARTUP/SHUTDOWN FILES
            section of zsh documentation accessible with <a href="ghcdotapp://man/zsh">'man zsh'</a>.
          </p>);
      default:
        return null;
    } }
  });

var Action = React.createClass({
  handleClick(e) {
    e.preventDefault();
    window.location = `ghcdotapp://${this.props.path}`;
  },
  render() {
    return <button onClick={this.handleClick}>{this.props.children}</button>;
  }
});


var App = React.createClass({
  componentDidMount() {
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
  },
  render() {
    var { paths
        , bundleVersion
        , bundlePath
        , inDownloadsDir
        , appName
        , environment
        , profiles
        } = this.props;
    var { shell
        , xcode
        } = paths;
    var profile = 'profile';
    if (shell === 'bash') {
      if (profiles.bash_profile) {
        profile = 'bash_profile';
      } else if (profiles.bash_login) {
        profile = 'bash_login';
      }
    } else if (shell === 'zsh') {
      profile = 'zshenv';
    }
    var t = profiles[profile]
      ? { buttonAction: 'Append to'
        , manualAction: 'append'
        , status: 'your existing'
        }
      : { buttonAction: 'Create'
        , manualAction: 'copy'
        , status: 'a new'
        };
    return (
      <div>
        <header>
          <img src="logo.svg" className="logo" />
          <p>This app is a container for GHC and cabal-install.</p>
          <p>To use it, move the application folder somewhere stable such as your
             Applications folder, and then add it to your <code>$PATH</code></p>
        </header>
        <section>
          <p>
            <Action path={`append-profile/${profile}`}>
              {t.buttonAction} ~/.{profile}
            </Action>{' '}
            or manually {t.manualAction} the following to {t.status}
            {' '}<code>~/.{profile}</code> file
          </p>
          <CodeBox bundleVersion={bundleVersion} bundlePath={bundlePath} />
          <Invocation shell={shell} />
          <nav>
            <Action path="refresh">Refresh Checklist</Action>
            <Action path="relaunch">Relaunch App</Action>
            <Action path="open-docs">Open GHC Docs</Action>
          </nav>
          <ul className="checklist">
            { !paths.shell
            ? <li className="warning">These instructions were written for users who use bash or zsh, not <code>{environment.SHELL}</code>. Hopefully you know what you're doing!</li>
            : null
            }
            { !paths.xcode
            ? <li className="warning">Xcode is not installed (<a href="ghcdotapp://install-xcode">Install Xcode</a>)</li>
            : <li className="check-mark">Xcode is installed at <code>{paths.xcode}</code></li>
            }
            { inDownloadsDir
            ? <li className="warning">{appName} (<a href="ghcdotapp://show-in-finder">Show in finder</a>) is in your Downloads folder. You should move it to a stable location such as your Applications folder and then <a href="ghcdotapp://relaunch">restart this application</a>. </li>
            : <li className="check-mark">{appName} looks like it's in a stable location at: <code>{bundlePath}</code></li>
            }
            { !paths.loginGHC
            ? <li className="warning">GHC not found on your <code>PATH</code>, follow the above instructions to add it.</li>
            : paths.loginGHC.indexOf(bundlePath) !== 0
            ? <li className="warning">Found another GHC on your <code>PATH</code> at: <code>{paths.loginGHC}</code>, follow the above instructions to override it.</li>
            : <li className="check-mark">Found this GHC on your <code>PATH</code> at: <code>{paths.loginGHC}</code></li>
            }
          </ul>
        </section>
      </div>);
  }
});

React.render(<App {...info} />, document.body);
