# fused-effects-optics

[![Hackage](https://img.shields.io/hackage/v/fused-effects-optics.svg)](https://hackage.haskell.org/package/fused-effects-optics)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)
[![Build Status](https://action-badges.now.sh/fused-effects/fused-effects-optics)](https://github.com/fused-effects/fused-effects-optics/actions)

This package provides an interface to the [`optics`](https://github.com/well-typed/optics) library is compatible with [`fused-effects`](https://github.com/robrix/fused-effects). The combinators provided by `optics-extra` for operating in monadic contexts—`gview`, `use`, `.=`, &c.—rely on `mtl` for `MonadState` and `MonadReader`, which is not applicable to `Reader` and `State` effects.

## License

BSD3, like `fused-effects`.
