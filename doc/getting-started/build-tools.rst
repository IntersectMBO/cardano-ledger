Build tools
-----------

For building LaTeX documents we use
```nix`` <https://nixos.org/nix/download.html>`__. Haskell files can be
built either with ``nix`` or
```stack`` <https://docs.haskellstack.org/en/stable/README/>`__.

When using ``nix`` it is recommended that you setup the cache, so that
it can reuse built artifacts, reducing the compilation times
dramatically:

If you are using `NixOS <https://nixos.org/>`__ add the snippet below to
your ``/etc/nixos/configuration.nix``:

::

   nix.binaryCaches = [
     "https://cache.nixos.org"
     "https://hydra.iohk.io"
   ];

   nix.binaryCachePublicKeys = [
     "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
   ];

If you are using the ``nix`` package manager next to another operating
system put the following in ``/etc/nix/nix.conf`` if you have a
system-wide ``nix`` installation , or in ``~/.config/nix/nix.conf`` if
you have a local installation:

::

   substituters        = https://hydra.iohk.io https://cache.nixos.org/
   trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
   