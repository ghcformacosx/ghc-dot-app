if [ -n "${BASH_VERSION}" ]; then
    echo "shell: bash"
    echo "loginGHC: $(bash -lc '/usr/bin/which ghc')"
    echo "loginPATH: $(bash -lc 'echo $PATH')"
    if [ -r !/.bashrc ]; then
        source ~/.bashrc
    fi
elif [ -n "${ZSH_VERSION}" ]; then
    echo "shell: zsh"
    echo "loginGHC: $(zsh -lc '/usr/bin/which ghc')"
    echo "loginPATH: $(zsh -lc 'echo $PATH')"
else
    echo "error: unsupported shell"
fi
echo "otherGHC: $(/usr/bin/which ghc)"
echo "otherPATH: $(echo $PATH)"
echo "xcode: $(xcode-select -p 2>/dev/null)"
