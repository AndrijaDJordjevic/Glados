FROM fpco/stack-build:lts-21.22

WORKDIR /app
COPY . .
RUN apt-get update && \
	apt-get install -y build-essential && \
	stack setup
