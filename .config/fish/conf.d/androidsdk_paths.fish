# Add paths to Android SDK tools if they're installed
#

if test -d $HOME/Android/Sdk
    set -a PATH $HOME/Android/Sdk/platform-tools
    set -a PATH $HOME/Android/Sdk/emulator
end
