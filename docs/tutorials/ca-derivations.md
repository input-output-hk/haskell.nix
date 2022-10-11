# Content addressed derivations
## Introduction

Floating content addressed derivations (from now *CA derivations*) is an experimental feature which substantially change how the hashes in the store paths are calculated.
Indeed, normally derivations are input addressed i.e. the outputs store paths depends only on the derivation inputs, instead with CA derivations they depend on the content of the outputs.

This has two main advantages:

- The so-called "early cutoff", namely the ability of Nix to stop a build if the build outputs would be something already built. 
For example suppose you add a comment in an Haskell source, at this point Nix will rebuild the component depending on this source but since the output will be the same (adding a comment is an "output-invariant" change for `ghc`) every other component that depends on that will not be rebuilt.
- Users of the same Nix store does not need to trust each other when using substituters.

You can find more information in the [ca-derivations page on the wiki](https://nixos.wiki/wiki/Ca-derivations) (and in the other resources linked there).

## Usage
### Enable CA derivations in your system
First of all your Nix installation must support the `ca-derivations` experimental feature, this can done by adding the following in your `nix.conf`:

```
experimental-features = ca-derivations
```

Or if you use NixOS:
```nix
nix.extraOptions = ''
    experimental-features = ca-derivations
'';
```

## Enable CA derivations in your project
At this point you can pass a new module to `project'` that tells `haskell.nix` to build every component in the project as CA derivation.

```nix
haskell-nix.project' {
	# ...
	
	modules = [{
		contentAddressed = true;
		# packages.project-name.components.exes.executable.contentAddressed = true;
	}];
};
```

Optionally you can also specify which components you don't want to be content addressed.

## Known problems
### Limitation of the current CA derivations implementation

As explained in the [RFC 62](https://github.com/tweag/rfcs/blob/cas-rfc/rfcs/0062-content-addressed-paths.md)

> The current implementation has a naive approach that just forbids fetching a path if the local system has a different realisation for the same drv output. This approach is simple and correct, but it's possible that it might not be good-enough in practice as it can result in a totally useless binary cache in some pathological cases.

For example, suppose that your machine builds a derivation `A` producing an output `A.out` in your store and that after that a CI machine builds the same derivation `A` but producing a different output `A.out'` and populating a cache with this output.
At this point, if you need to build a derivation `B` that depends on `A`, since you already have the realisation `A.out` in your local store and you can't get `B.out` from the cache and you will end up building `B` even if one of its realisation is in the cache.

This means that, in some cases, enabling CA derivations would lead to more rebuilds than not having it.

### Hydra
Hydra currently doesn't support CA derivations, efforts are being made in this direction.


### GHC is not deterministic
Currently `ghc` is determinstic only disabling the parallel building i.e. passing `-j1`. [Here](https://gitlab.haskell.org/ghc/ghc/-/issues/12935) the upstream issue.

Having a deterministic `ghc` would be a dream since it will automatically fix all the pathological cases about substituters discussed above and would allow `haskell.nix` to parallel build even when using CA derivations.
