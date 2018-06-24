# squeal

A Haskell command line wrapper around [haveibeenpwned](https://haveibeenpwned.com/)

<img src="https://cdn-images.threadless.com/threadless-media/artist_shops/shops/mschulz/products/543391/shirt-1525831426-c3a864fd1617ef517aaba968668605f6.png?v=3&d=eyJvbmx5X21ldGEiOiBmYWxzZSwgImZvcmNlIjogZmFsc2UsICJvcHMiOiBbWyJ0cmltIiwgW2ZhbHNlLCBmYWxzZV0sIHt9XSwgWyJyZXNpemUiLCBbXSwgeyJ3aWR0aCI6IDk5Ni4wLCAiYWxsb3dfdXAiOiBmYWxzZSwgImhlaWdodCI6IDk5Ni4wfV0sIFsiY2FudmFzX2NlbnRlcmVkIiwgWzEyMDAsIDEyMDBdLCB7ImJhY2tncm91bmQiOiAiZmZmZmZmIn1dLCBbInJlc2l6ZSIsIFsxNjAwXSwge31dLCBbImNhbnZhc19jZW50ZXJlZCIsIFsxNjAwLCAxNjAwLCAiI2ZmZmZmZiJdLCB7fV0sIFsiZW5jb2RlIiwgWyJqcGciLCA4NV0sIHt9XV19" width="50%" height="50%" />

art by [mschulz](https://www.threadless.com/discover/s/mschulz)

## Usage

```
squeal -e <email_address> | -p <password>
```

To list breaches for an email address use:

```
squeal -e your-email@domain.com
```

which will return an output like:

```
Breached Accounts:
 - Site1
 - Site2
 - Site3
```

To verify a password has not been pwned use:


```
squeal -p your-password
```

which will return an output like:

```
Password stolen: True
```

## Building

run `build`

## Installing

run `build-install` to install squeal to ~/.local/bin.

## Running (without installing)

run:

```
run -e <email_address> | -p <password>
```
