#!/usr/bin/env python

import time

# Normal comment. Nothing to see here.

# ! Important annotation!

# x This code shouldn't be here
time.clock()


def main():
    now = time.clock()
    # ? Should I do something here?
    print time.clock() - now

# TODO: Do something!
if __name__ == '__main__':
    main()
