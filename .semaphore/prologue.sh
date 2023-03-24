#!/bin/bash

# Set proper permissions on the SSH key and load it into the SSH agent.
# Without the permissions, SSH refuses to work with this file.
chmod 0600 ~/.ssh/id_ed25519
ssh-add ~/.ssh/id_ed25519

mount_nix_store() {
  # Before we can successfully restore to `/nix` it needs to be created and owned
  # by the CI user. Without this, the `cache restore` command fails because it
  # doesn't have permission to create `/nix`. (We cannot run the cache restore
  # command as `root` as it takes settings from environment variables.)
  # We use the local scratch SSD mounted at `/mnt` to prevent running out of disk
  # space, as the Nix store takes up about 10 GiB when uncompressed, and a build
  # on the smallest Semaphore instance starts out with only 13 GiB of disk space,
  # of which 4 GiB is used by the cloned repository.
  # The SSD has (at the time of writing) 80 GiB of space, of which 32 GiB is free
  # when a build starts. If we really want to we can also consider wiping the SSD
  # before using it, we don't need to do this yet.
  sudo mkdir -p /mnt/nix /nix
  sudo mount --bind /mnt/nix /nix
  sudo chown -R semaphore:semaphore /nix
}
mount_nix_store
