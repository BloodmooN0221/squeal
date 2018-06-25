# squeal

A Haskell command line wrapper around [haveibeenpwned](https://haveibeenpwned.com/)

![squeal](squeal.jpg)

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
