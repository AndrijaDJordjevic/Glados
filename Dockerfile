FROM fpco/stack-build:lts-21.22

WORKDIR /app
COPY . .

RUN apt-get update && \
	apt-get install -y build-essential && \
	apt-get install -y python3 python3-pip && \
	pip3 install termcolor
