proj_name="station-web"
docker_cmd="docker-compose -p ${proj_name} -f docker-compose.dev.yml"

# Build station-web image
function sm_build() {
  $docker_cmd build
}

# Run postgres, pgadmin and station-web **attached**
function sm_run() {
  $docker_cmd up
}

# Run postgres, pgadmin and station-web **detached**
function sm_run_detached() {
  $docker_cmd up -d
}

# See logs up to the time this command is executed. Accepts service name as the only argument. Give none to see all logs. Service names are listed in docker-compose.yml
function sm_logs() {
  $docker_cmd logs $1
}

# Follow logs of the services. Accepts one service name as the only argument. Give none to follow all logs. Service names are listed in docker-compose.yml
function sm_logsf() {
  $docker_cmd logs -f $1
}

# Start an interactive shell in station web it's where we can execute the cabal commands
function sm_shell() {
  docker exec -it "${proj_name}_web_1" /bin/bash
}

# Restart services. Accepts one service name as the only argument. Give none to restart all services. Service names are listed in docker-compose.yml
function sm_restart() {
  $docker_cmd restart $1
}