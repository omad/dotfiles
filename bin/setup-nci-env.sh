#!/bin/bash

set -xe



function get_download_url {
  curl -sL  https://api.github.com/repos/$1/$2/releases/latest | jq -r ".assets[] | select(.browser_download_url | contains(\"$3\")) | .browser_download_url"
}

function download_if_newer {
    url=$1
    filename=$2
    http_code="$(curl -z ${filename} -O ${url} --silent --location --write-out %{http_code})"

    echo $http_code
#    return [[ $http_code == 200 ]]
}

function install_rust_util {
    pushd $TMPDIR

    url=$(get_download_url $1 $2 x86_64-unknown-linux-musl)
    http_code=$(download_if_newer ${url} $3)

    if [[ "$http_code" == "200" ]]; then
      # code here to process index.html because 200 means it gets updated
      tar xzf $2*
      find $2* -executable -type f -exec cp {} ~/bin/ \;
      find $2* -name '*.1' -exec cp {} ~/share/man/man1 \;
      find $2* -name '*.bash' -exec cp {} ~/.bash/ \;
    fi

    popd
}

install_rust_util sharkdp bat ~/bin/bat
install_rust_util sharkdp fd ~/bin/fd
install_rust_util BurntSushi ripgrep ~/bin/rg



curl -L -z ${filename} -o ~/bin/plantuml.jar 'https://downloads.sourceforge.net/project/plantuml/plantuml.jar?r=https%3A%2F%2Fsourceforge.net%2Fprojects%2Fplantuml%2Ffiles%2Fplantuml.jar%2Fdownload&ts=1563928325'



cat <<EOT > $HOME/bin/plantuml
#!/bin/sh -e
java -jar $HOME/bin/plantuml.jar "$@"
EOT
chmod +x $HOME/bin/plantuml

cd $TMPDIR
curl -L https://github.com/github/hub/releases/download/v2.12.3/hub-linux-amd64-2.12.3.tgz | tar -xz
cd hub-*
prefix=$HOME ./install


# Emacs
cd $TMPDIR
wget https://mirror.freedif.org/GNU/emacs/emacs-26.2.tar.xz
tar xf emacs-26.2*
cd emacs-26.2
./configure --prefix=$HOME
make -j 8
make install


# Dropbox
cd ~ && wget -O - "https://www.dropbox.com/download?plat=lnx.x86_64" | tar xzf -

Next, run the Dropbox daemon from the newly created .dropbox-dist folder.

~/.dropbox-dist/dropboxd

