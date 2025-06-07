#!/bin/bash
# FlowLoom Docker Isolated Environment Script
# Provides secure, network-isolated Claude Code execution with --dangerously-skip-permissions

set -euo pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
DOCKER_DIR="${PROJECT_ROOT}/flowloom-docker"
COMMAND="${1:-help}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if Docker is available
check_docker() {
    if ! command -v docker &> /dev/null; then
        log_error "Docker is not installed or not in PATH"
        echo "Please install Docker Desktop from https://www.docker.com/products/docker-desktop"
        exit 1
    fi
    
    if ! COMPOSE_FILE=docker-compose.yml docker compose version &> /dev/null; then
        log_error "Docker Compose v2 is required"
        echo "Please update to Docker Compose v2 (comes with Docker Desktop)"
        exit 1
    fi
    
    # Check if Docker daemon is running
    if ! docker info &> /dev/null; then
        log_error "Docker daemon is not running"
        echo "Please start Docker Desktop and try again"
        exit 1
    fi
}

# Set up the Docker environment
setup_environment() {
    log_info "Setting up FlowLoom Docker environment..."
    
    # Ensure Docker directory exists
    if [ ! -d "${DOCKER_DIR}" ]; then
        log_error "FlowLoom Docker directory not found at ${DOCKER_DIR}"
        echo "Please ensure you're running this from a FlowLoom project directory"
        exit 1
    fi
    
    # Run volume setup script
    log_info "Configuring volume mounts and permissions..."
    cd "${DOCKER_DIR}"
    
    if [ -x "./scripts/setup-volumes.sh" ]; then
        ./scripts/setup-volumes.sh
    else
        log_warning "Volume setup script not found or not executable"
        log_info "Creating minimal .env file..."
        cat > .env << EOF
# FlowLoom Docker environment configuration
FLOWLOOM_UID=$(id -u)
FLOWLOOM_GID=$(id -g)
PROJECT_DIR=${PWD}
EOF
    fi
}

# Build the Docker image if needed
ensure_image() {
    log_info "Checking Docker image..."
    
    cd "${DOCKER_DIR}"
    
    # Check if image exists
    if ! docker images | grep -q "flowloom/claude-isolated"; then
        log_info "Building FlowLoom Docker image (this may take a few minutes)..."
        COMPOSE_FILE=docker-compose.yml docker compose build
        log_success "Docker image built successfully"
    else
        log_info "Docker image already exists"
    fi
}

# Start the isolated container
start_container() {
    check_docker
    setup_environment
    ensure_image
    
    log_info "Starting FlowLoom isolated container..."
    
    cd "${DOCKER_DIR}"
    
    # Check if container is already running
    if COMPOSE_FILE=docker-compose.yml docker compose ps -q flowloom-claude | grep -q .; then
        log_warning "Container is already running"
        log_info "Connecting to existing container..."
    else
        # Start the container
        COMPOSE_FILE=docker-compose.yml docker compose up -d
        
        # Wait a moment for startup
        sleep 2
        
        # Verify container is running
        if ! COMPOSE_FILE=docker-compose.yml docker compose ps -q flowloom-claude | grep -q .; then
            log_error "Failed to start container"
            COMPOSE_FILE=docker-compose.yml docker compose logs flowloom-claude
            exit 1
        fi
        
        log_success "Container started successfully"
    fi
    
    # Show connection info
    echo ""
    log_info "FlowLoom Docker Environment Ready!"
    echo "=================================="
    echo "• Network isolation: ✓ (no internet access)"
    echo "• Claude Code available: ✓ (with --dangerously-skip-permissions)"
    echo "• Workspace mounted: ✓ (at /workspace)"
    echo ""
    echo "Starting interactive session..."
    echo ""
    
    # Connect to the container
    COMPOSE_FILE=docker-compose.yml docker compose exec flowloom-claude /bin/bash
}

# Stop the container
stop_container() {
    log_info "Stopping FlowLoom isolated container..."
    
    cd "${DOCKER_DIR}"
    
    if COMPOSE_FILE=docker-compose.yml docker compose ps -q flowloom-claude | grep -q .; then
        COMPOSE_FILE=docker-compose.yml docker compose down
        log_success "Container stopped successfully"
    else
        log_warning "Container is not running"
    fi
}

# Show container status
show_status() {
    log_info "FlowLoom Docker Environment Status"
    echo "=================================="
    
    cd "${DOCKER_DIR}"
    
    # Check if container is running
    if COMPOSE_FILE=docker-compose.yml docker compose ps -q flowloom-claude 2>/dev/null | grep -q .; then
        echo -e "Status: ${GREEN}RUNNING${NC}"
        echo ""
        echo "Container details:"
        COMPOSE_FILE=docker-compose.yml docker compose ps
        echo ""
        echo "Network isolation test:"
        if COMPOSE_FILE=docker-compose.yml docker compose exec -T flowloom-claude curl -s --connect-timeout 2 http://8.8.8.8 &>/dev/null; then
            echo -e "Network isolation: ${RED}FAILED${NC} (container has internet access)"
        else
            echo -e "Network isolation: ${GREEN}ACTIVE${NC} (no internet access)"
        fi
    else
        echo -e "Status: ${YELLOW}STOPPED${NC}"
    fi
    
    echo ""
    echo "Available commands:"
    echo "  $0 start   - Start isolated environment"
    echo "  $0 stop    - Stop container"
    echo "  $0 status  - Show this status"
    echo "  $0 shell   - Connect to running container"
    echo "  $0 claude  - Run Claude Code in container"
}

# Connect to running container
connect_shell() {
    cd "${DOCKER_DIR}"
    
    if ! COMPOSE_FILE=docker-compose.yml docker compose ps -q flowloom-claude | grep -q .; then
        log_error "Container is not running"
        echo "Run '$0 start' to start the container first"
        exit 1
    fi
    
    log_info "Connecting to FlowLoom isolated container..."
    COMPOSE_FILE=docker-compose.yml docker compose exec flowloom-claude /bin/bash
}

# Run Claude Code in the container
run_claude() {
    cd "${DOCKER_DIR}"
    
    if ! COMPOSE_FILE=docker-compose.yml docker compose ps -q flowloom-claude | grep -q .; then
        log_error "Container is not running"
        echo "Run '$0 start' to start the container first"
        exit 1
    fi
    
    # Pass any additional arguments to Claude
    shift # Remove 'claude' from arguments
    
    log_info "Running Claude Code in isolated container..."
    echo ""
    echo "If Claude Code fails to authenticate:"
    echo "1. Set ANTHROPIC_API_KEY in flowloom-docker/.env"
    echo "2. Or run: claude auth login (inside container)"
    echo ""
    
    if [ $# -eq 0 ]; then
        # Interactive mode
        COMPOSE_FILE=docker-compose.yml docker compose exec flowloom-claude claude --dangerously-skip-permissions
    else
        # Pass arguments to Claude
        COMPOSE_FILE=docker-compose.yml docker compose exec flowloom-claude claude --dangerously-skip-permissions "$@"
    fi
}

# Show help
show_help() {
    echo "FlowLoom Docker Isolated Environment"
    echo "===================================="
    echo ""
    echo "Provides secure, network-isolated Claude Code execution with --dangerously-skip-permissions"
    echo ""
    echo "USAGE:"
    echo "  $0 <command> [options]"
    echo ""
    echo "COMMANDS:"
    echo "  start     Start the Docker environment"
    echo "  stop      Stop the Docker container"
    echo "  status    Show container status and health"
    echo "  shell     Connect to the running container shell"
    echo "  claude    Run Claude Code in the container"
    echo "  connect   Switch to selective network (Anthropic API access)"
    echo "  isolate   Switch to fully isolated network (no internet)"
    echo "  help      Show this help message"
    echo ""
    echo "EXAMPLES:"
    echo "  $0 connect && $0 start      # Enable API access for authentication"
    echo "  $0 claude                   # Run Claude interactively"
    echo "  $0 isolate && $0 start      # Switch to full isolation"
    echo "  $0 shell                    # Get bash shell in container"
    echo ""
    echo "SECURITY FEATURES:"
    echo "  • Dual network modes: full isolation or selective API access"
    echo "  • Non-root container execution"
    echo "  • Minimal Linux capabilities"
    echo "  • Read-only system files"
    echo "  • Project directory mounted at /workspace"
    echo ""
    echo "NETWORK MODES:"
    echo "  • selective: Allows Anthropic API access for authentication"
    echo "  • isolated:  Complete network isolation (no internet access)"
    echo ""
    echo "For more information, see: flowloom-docker/README.md"
}

# Main command dispatcher
case "${COMMAND}" in
    start)
        start_container
        ;;
    stop)
        stop_container
        ;;
    status)
        show_status
        ;;
    shell)
        connect_shell
        ;;
    claude)
        run_claude "$@"
        ;;
    isolate)
        log_info "Switching to fully isolated network (no internet access)..."
        cd "${DOCKER_DIR}"
        # Stop container if running
        if COMPOSE_FILE=docker-compose.yml docker compose ps -q flowloom-claude 2>/dev/null | grep -q .; then
            COMPOSE_FILE=docker-compose.yml docker compose down
        fi
        # Update network in compose file
        sed -i '' 's/- selective/- isolated/' docker-compose.yml
        log_success "Switched to isolated network mode"
        echo "Use './bin/flowloom-docker.sh start' to restart with full isolation"
        ;;
    connect)
        log_info "Switching to selective network (Anthropic API access only)..."
        cd "${DOCKER_DIR}"
        # Stop container if running
        if COMPOSE_FILE=docker-compose.yml docker compose ps -q flowloom-claude 2>/dev/null | grep -q .; then
            COMPOSE_FILE=docker-compose.yml docker compose down
        fi
        # Update network in compose file
        sed -i '' 's/- isolated/- selective/' docker-compose.yml
        log_success "Switched to selective network mode"
        echo "Use './bin/flowloom-docker.sh start' to restart with API access"
        ;;
    help|--help|-h)
        show_help
        ;;
    *)
        log_error "Unknown command: ${COMMAND}"
        echo ""
        show_help
        exit 1
        ;;
esac