# squeal

A Haskell command line wrapper around [haveibeenpwned](https://haveibeenpwned.com/)

![squeal](squeal.jpg)


Latest version: [v0.1.0.1](https://github.com/ssanj/squeal/releases/tag/v0.1.0.1)

## Usage

```
squeal -e <email_address> | -p <password> | -ef <email_address_file>
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

To verify a password has not been pwned use:


```
squeal -p your-password
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


## Building

Build with:

```build```

Build and install (to ~/.local/bin) with:

```build-install```


## Running (without installing)

```
run -e <email_address> | -p <password>
```
