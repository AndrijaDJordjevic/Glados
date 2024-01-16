# GLADOS

## Description

The goal of this project is to implement a programming language of your own design, in Haskell.

## Table of Contents

- Commit Message Format
- Installation
- Usage
- Technologies Used
- License
- Authors
- Project Status

## Commit Message Format

- [ADD] Add a new feature
- [FIX] Fix a bug
- [UPD] Update code or feature
- [RM] Remove code or feature

## Installation

Clone the repository

```bash
git clone sshkey or https
```

### Install docker

#### for linux

```bash
sudo apt-get update && sudo apt-get install -y apt-transport-https ca-certificates curl software-properties-common
```

Ajoutez la clé GPG officielle de Docker :

```bash
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
```

Ajoutez le dépôt Docker stable :

```bash
echo "deb [signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
```

Installez Docker :

```bash
sudo apt-get update && sudo apt-get install -y docker-ce docker-ce-cli containerd.io
```

Vérifiez que Docker a été installé correctement en exécutant une commande simple :

```bash
sudo docker --version
```

#### Pour Windows and macOS:

Vous pouvez utiliser Docker Desktop pour Windows, disponible sur le site officiel de Docker : https://www.docker.com/products/docker-desktop/

### Run the project

Build docker image:

```bash
make build
```

Run docker image:

```bash
make run
```

## Usage

This project uses a Makefile to streamline common tasks. Before pushing your changes or **merging a pull request**, make sure to follow these steps:

### 1. Run Unit Tests

Before pushing your changes, make sure all unit tests pass by running the following command:

```bash
make tests
```

### 2. Push and Pull Request

Once all the above steps have been successfully completed, you can push your changes to the development branch. Also, make sure that the CI/CD pipeline passes successfully before merging your pull request.

Thank you for following these steps to maintain code quality and avoid test-related issues when merging changes.

## Technologies Used

Haskell, Docker, Makefile, Github actions

## License

## Contributing

You cannot contribute to this project.

## Authors

#### Louis Sappey

#### Andrija Djorjevic

#### Alexandre Cathalifaud

#### Hugo Groschaus

#### Martin Petit

## Project Status

In development
