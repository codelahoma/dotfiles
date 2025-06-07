"""FlowLoom Configuration System.

This package provides configuration management for FlowLoom,
including hierarchical loading, validation, and runtime access.
"""

# Import main configuration classes
from .models import (
    FlowLoomConfig,
    ProjectConfig,
    AgentConfig,
    ResourceConfig,
    WorkflowConfig,
    ComplianceConfig,
    SecurityConfig,
    EnvironmentConfig
)

# Import configuration loader
from .loader import (
    ConfigurationLoader,
    ConfigurationSource,
    LoadResult,
    EnvironmentVariableResolver,
    ConfigurationCache,
    load_config,
    get_loader
)

# Import validation
from .schema import (
    ConfigurationSchema,
    ConfigurationValidator,
    ValidationResult
)

# Version information
__version__ = "1.0.0"

# Main exports
__all__ = [
    # Core configuration
    "FlowLoomConfig",
    "ProjectConfig", 
    "AgentConfig",
    "ResourceConfig", 
    "WorkflowConfig",
    "ComplianceConfig",
    "SecurityConfig",
    "EnvironmentConfig",
    
    # Loading system
    "ConfigurationLoader",
    "ConfigurationSource",
    "LoadResult",
    "EnvironmentVariableResolver",
    "ConfigurationCache",
    
    # Validation system
    "ConfigurationSchema",
    "ConfigurationValidator",
    "ValidationResult",
    
    # Convenience functions
    "load_config",
    "get_loader"
]