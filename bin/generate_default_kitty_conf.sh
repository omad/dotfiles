#!/usr/bin/env kitty +launch
from kitty.config import commented_out_default_config as conf
from kitty.constants import str_version
import sys

if __name__ == '__main__':
    if len(sys.argv) > 1:
        file_path = sys.argv[1]
    # file_path = f'kitty.{str_version}.default.conf'
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(conf())
        print(file_path)
    else:
        print(conf())
