# squeal

A Haskell command line wrapper around [haveibeenpwned](https://haveibeenpwned.com/)

![squeal](squeal.jpg)

[![Build Status](https://travis-ci.org/ssanj/squeal.svg?branch=master)](https://travis-ci.org/ssanj/squeal)

Latest version: [v0.2.0.1](https://github.com/ssanj/squeal/releases/tag/v0.2.0.1)

## Usage

```
Usage: squeal -v | -e  <email> | -ef <email_address_file> (apiDelay) | -p
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

To use a file with multiple emails (each on separate lines) use:

```
squeal -ef your-email-file
```

> Requests to the breaches and pastes APIs are limited to one per every 1500 milliseconds.

Because of this squeal defaults to sending breach requests every 1600ms. You can override this value by specifying another request delay. For example to use a 1800ms delay instead use:

```
squeal -ef your-email-file 1800
```

To verify a password has not been pwned use:


```
squeal -p
```

which will return an output like:

```
Password stolen: True
```

## Installing

### Mac

Install via [brew](https://brew.sh/):

```
brew tap ssanj/homebrew-squeal
brew install squeal
```

Alternatively, download the [latest release](https://github.com/ssanj/squeal/releases/) from the releases page.

### Linux

Download the [latest release](https://github.com/ssanj/squeal/releases/) from releases page.

## Upgrading

### Mac

Simply upgrade through *brew*:

```
brew upgrade --cleanup squeal
```

## Building

Build with:

```build```

Build and install (to ~/.local/bin) with:

```build-install```


## Running (without installing)

```
run -v | -e  <email> | -ef <email_address_file> (apiDelay) | -p
```

## Releasing

- Bump *version* in package.yaml: `X.Y.Z`
- make changes
- commit changes
- tag changes to match version: `git tag 'vX.Y.Z'`
- push commit
- push tags: `git push --tags`
- update README (this file) with latest version link
- push commit

### OSX

- Grab SHA256 of *squeal-vX.Y.Z-osx.tar.gz* from [Travis](https://travis-ci.org/ssanj/squeal/builds)
- Update [homebrew tap](https://github.com/ssanj/homebrew-squeal) with latest version and hash.
- commit and push

