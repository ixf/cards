
wget -O- https://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc | apt-key add -
echo "deb https://packages.erlang-solutions.com/ubuntu bionic contrib" | tee /etc/apt/sources.list.d/rabbitmq.list
apt update
apt -y install erlang gcc g++
wget https://github.com/erlang/rebar3/releases/download/3.9.1/rebar3
chmod +x rebar3
./rebar3 get-deps
