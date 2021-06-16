# Defined in /tmp/fish.rINInR/update-zoom.fish @ line 2
function update-zoom


    set -l latest_version (curl -Ls https://zoom.us/download\?os\=linux |   pup '.linux-ver-text text{}' | awk -F'[ ().]' '{printf $2"."$3"."$6"."$7"\n"}')
    set -l installed_version (dpkg-query -f='${Version}' -W zoom)

    if dpkg --compare-versions $installed_version lt $latest_version
        echo A newer version of zoom.us is available, downloading and installing
        pushd (mktemp -d)
        wget https://cdn.zoom.us/prod/$latest_version/zoom_amd64.deb
        sudo dpkg -I zoom_amd64.deb
        popd
    end

end
