#!/bin/bash

trap ctrl_c INT
function ctrl_c() {
echo "** Trapped CTRL-C"
exit 1
}

brew_command=/usr/local/bin/brew
brew_cask_command="$brew_command cask"
failed_items=""
function install_package() {
echo EXECUTING: brew install $1 $2
$brew_command install $1 $2
[ $? -ne 0 ] && $failed_items="$failed_items $1"  # package failed to install.
}
function install_cask_package() {
echo EXECUTING: brew cask install $1
$brew_cask_command install $1
[ $? -ne 0 ] && $failed_items="$failed_items $1"  # package failed to install.
}
/usr/local/bin/brew tap cartr/qt4
/usr/local/bin/brew tap d12frosted/emacs-plus
/usr/local/bin/brew tap homebrew/cask
/usr/local/bin/brew tap homebrew/cask-fonts
/usr/local/bin/brew tap homebrew/cask-versions
/usr/local/bin/brew tap homebrew/command-not-found
/usr/local/bin/brew tap homebrew/core
/usr/local/bin/brew tap homebrew/services
/usr/local/bin/brew tap kylef/formulae
/usr/local/bin/brew tap vapor/tap
install_package adns ''
install_package ascii ''
install_package autoconf ''
install_package automake ''
install_package awscli ''
install_package bdw-gc ''
install_package cairo ''
install_package cloc ''
install_package cmake ''
install_package coreutils ''
install_package cowsay ''
install_package cscope ''
install_package ctags ''
install_package ctls ''
install_package dbus ''
install_package direnv ''
install_package emacs ''
install_package emacs-plus ''
install_package fasd ''
install_package fd ''
install_package fontconfig ''
install_package freetds ''
install_package freetype ''
install_package fribidi ''
install_package gcc ''
install_package gd ''
install_package gdbm ''
install_package gdk-pixbuf ''
install_package geckodriver ''
install_package gettext ''
install_package ghc ''
install_package ghostscript ''
install_package git ''
install_package git-lfs ''
install_package glib ''
install_package global ''
install_package gmp ''
install_package gnu-sed ''
install_package gnu-time ''
install_package gnupg ''
install_package gnutls ''
install_package gobject-introspection ''
install_package graphite2 ''
install_package graphviz ''
install_package gti ''
install_package gv ''
install_package harfbuzz ''
install_package highlight ''
install_package homeshick ''
install_package htop ''
install_package hub ''
install_package icu4c ''
install_package ilmbase ''
install_package imagemagick ''
install_package imagemagick@6 ''
install_package isl ''
install_package ispell ''
install_package jpeg ''
install_package jq ''
install_package jrnl ''
install_package libassuan ''
install_package libcroco ''
install_package libde265 ''
install_package libevent ''
install_package libffi ''
install_package libgcrypt ''
install_package libgpg-error ''
install_package libheif ''
install_package libidn2 ''
install_package libksba ''
install_package libmpc ''
install_package libomp ''
install_package libpng ''
install_package libressl ''
install_package librsvg ''
install_package libtasn1 ''
install_package libtiff ''
install_package libtool ''
install_package libunistring ''
install_package libusb ''
install_package libyaml ''
install_package libzip ''
install_package little-cms2 ''
install_package llvm ''
install_package lua ''
install_package lua@5.1 ''
install_package luarocks ''
install_package lz4 ''
install_package lzo ''
install_package md5sha1sum ''
install_package mercurial ''
install_package mpfr ''
install_package mysql ''
install_package nettle ''
install_package nmap ''
install_package npth ''
install_package nspr ''
install_package nss ''
install_package oniguruma ''
install_package openblas ''
install_package openexr ''
install_package openjpeg ''
install_package openssl ''
install_package openssl@1.1 ''
install_package openvpn ''
install_package p11-kit ''
install_package pandoc ''
install_package pango ''
install_package pcre ''
install_package pcre2 ''
install_package pinentry ''
install_package pipenv ''
install_package pixman ''
install_package pkg-config ''
install_package plantuml ''
install_package poppler ''
install_package postgresql ''
install_package pyenv ''
install_package pyenv-virtualenv ''
install_package python ''
install_package python@2 ''
install_package r ''
install_package readline ''
install_package reattach-to-user-namespace ''
install_package redis ''
install_package ripgrep ''
install_package rtags ''
install_package shared-mime-info ''
install_package shpotify ''
install_package sl ''
install_package sqlite ''
install_package swiftenv ''
install_package texinfo ''
install_package the_silver_searcher ''
install_package tldr ''
install_package tmux ''
install_package unbound ''
install_package unixodbc ''
install_package unoconv ''
install_package vapor ''
install_package w3m ''
install_package watchexec ''
install_package webp ''
install_package wget ''
install_package x265 ''
install_package xz ''
install_package zeromq ''
install_cask_package aerial
install_cask_package betterzip
install_cask_package chromedriver
install_cask_package dropbox
install_cask_package firefox-developer-edition
install_cask_package firefox-esr
install_cask_package font-hack-nerd-font
install_cask_package font-iosevka
install_cask_package font-iosevka-nerd-font
install_cask_package font-source-code-pro
install_cask_package growlnotify
install_cask_package java8
install_cask_package keycastr
install_cask_package launchrocket
install_cask_package mactex
install_cask_package multifirefox
install_cask_package osxfuse
install_cask_package qlcolorcode
install_cask_package qlimagesize
install_cask_package qlmarkdown
install_cask_package qlstephen
install_cask_package qlvideo
install_cask_package quicklook-json
install_cask_package quicklookase
install_cask_package qutebrowser
install_cask_package r-app
install_cask_package spotify
install_cask_package squirrelsql
install_cask_package suspicious-package
install_cask_package tunnelblick
install_cask_package vagrant
install_cask_package vagrant-manager
install_cask_package virtualbox
install_cask_package vlc
install_cask_package webpquicklook
install_cask_package xquartz
[ ! -z $failed_items ] && echo The following items were failed to install: && echo $failed_items
