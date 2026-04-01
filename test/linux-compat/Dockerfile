FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y \
    zsh \
    git \
    curl \
    wget \
    emacs-nox \
    tmux \
    build-essential \
    ripgrep \
    fd-find \
    fzf \
    bat \
    eza \
    jq \
    w3m \
    pass \
    gnupg \
    python3 \
    python3-pip \
    python3-venv \
    maildir-utils \
    isync \
    zoxide \
    zsh-syntax-highlighting \
    locales \
    sudo \
    && rm -rf /var/lib/apt/lists/*

# Generate locale (needed for zsh/emacs)
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && locale-gen
ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

# Create non-root test user with zsh
RUN useradd -m -s /bin/zsh testuser

# Copy test script
COPY test-dotfiles.sh /usr/local/bin/test-dotfiles.sh
RUN chmod +x /usr/local/bin/test-dotfiles.sh

USER testuser
WORKDIR /home/testuser

ENTRYPOINT ["/usr/local/bin/test-dotfiles.sh"]
