#!/bin/bash

ls -alh *.rec|cut -c17-20,38-|sort --numeric |tail
