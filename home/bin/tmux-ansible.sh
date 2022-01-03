#!/usr/bin/env bash
read -p "Enter Query: " query

tmux splitw -h bash -c "ansible-doc $query"
