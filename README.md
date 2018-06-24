# squeal

Haskell commandline wrapper around [haveibeenpwned](https://haveibeenpwned.com/)

## Usage

```
squeal -e <email_address> | -p <password>
```

For example to list breaches for an email address use:

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

For example to verify a password has not been pwned use:


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

run `build-install` to install squeal to ~/.local/bin. Ensure ~/.local/bin is on your path if you want to run squeal directly from there.

## Running (without installing)

run:

```
run -e <email_address> | -p <password>
```
