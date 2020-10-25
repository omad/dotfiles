# Defined in - @ line 1
function icat --wraps='kitty +kitten icat' --description 'alias icat=kitty +kitten icat'
  kitty +kitten icat $argv;
end
