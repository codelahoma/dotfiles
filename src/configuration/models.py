"""
Configuration data models using Pydantic for validation and type safety.
"""

from typing import Optional, Dict, Any, List
from pydantic import BaseModel, Field, validator
from datetime import timedelta
import re


class AgentConfig(BaseModel):
    """Multi-agent coordination configuration."""
    
    max_concurrent: int = Field(3, ge=1, le=10, description="Maximum concurrent agents")
    coordination_strategy: str = Field("hierarchical", description="Coordination strategy")
    default_model: str = Field("claude-sonnet-4", description="Default Claude model")
    memory_system: str = Field("dual", description="Memory system type")
    agent_timeout: str = Field("30m", description="Agent timeout duration")
    cross_agent_sharing: bool = Field(True, description="Enable cross-agent memory sharing")
    
    @validator('coordination_strategy')
    def validate_coordination_strategy(cls, v):
        allowed = ['hierarchical', 'peer-to-peer', 'centralized']
        if v not in allowed:
            raise ValueError(f"coordination_strategy must be one of {allowed}")
        return v
    
    @validator('memory_system')
    def validate_memory_system(cls, v):
        allowed = ['dual', 'mcp-only', 'monitor-only']
        if v not in allowed:
            raise ValueError(f"memory_system must be one of {allowed}")
        return v
    
    @validator('agent_timeout')
    def validate_timeout(cls, v):
        # Validate timeout format (e.g., "30m", "1h", "2h30m")
        pattern = r'^(\d+[hm])+$'
        if not re.match(pattern, v):
            raise ValueError("agent_timeout must be in format like '30m', '1h', '2h30m'")
        return v


class RateLimitingConfig(BaseModel):
    """Rate limiting configuration."""
    
    global_requests_per_minute: int = Field(100, ge=10, le=1000)
    per_agent_requests_per_minute: int = Field(30, ge=5, le=200)
    burst_allowance: int = Field(10, ge=1, le=50)


class MemoryConfig(BaseModel):
    """Memory management configuration."""
    
    retention_period: str = Field("7d", description="Memory retention period")
    max_context_window: str = Field("200k", description="Maximum context window")
    context_strategy: str = Field("truncate-oldest", description="Context management strategy")
    
    @validator('context_strategy')
    def validate_context_strategy(cls, v):
        allowed = ['truncate-oldest', 'truncate-newest', 'summarize', 'compress']
        if v not in allowed:
            raise ValueError(f"context_strategy must be one of {allowed}")
        return v


class RetryPolicyConfig(BaseModel):
    """Retry policy configuration."""
    
    max_retries: int = Field(3, ge=0, le=10)
    backoff_strategy: str = Field("exponential", description="Backoff strategy")
    base_delay: str = Field("1s", description="Base delay for retries")
    
    @validator('backoff_strategy')
    def validate_backoff_strategy(cls, v):
        allowed = ['linear', 'exponential', 'fixed']
        if v not in allowed:
            raise ValueError(f"backoff_strategy must be one of {allowed}")
        return v


class ResourceConfig(BaseModel):
    """Resource management configuration."""
    
    rate_limiting: RateLimitingConfig = Field(default_factory=RateLimitingConfig)
    memory: MemoryConfig = Field(default_factory=MemoryConfig)
    retry_policy: RetryPolicyConfig = Field(default_factory=RetryPolicyConfig)


class SyncConfig(BaseModel):
    """Sync workflow configuration."""
    
    strategy: str = Field("comprehensive", description="Sync strategy")
    auto_commit: bool = Field(True, description="Automatically commit changes")
    generate_commit_messages: bool = Field(True, description="Generate commit messages")
    push_on_sync: bool = Field(True, description="Push to remote on sync")
    backup_configs: bool = Field(True, description="Backup configurations")
    
    @validator('strategy')
    def validate_strategy(cls, v):
        allowed = ['minimal', 'comprehensive', 'selective']
        if v not in allowed:
            raise ValueError(f"strategy must be one of {allowed}")
        return v


class PlanningConfig(BaseModel):
    """Planning workflow configuration."""
    
    auto_numbering: bool = Field(True, description="Auto-number plans")
    plan_prefix: str = Field("FlowLoom", description="Plan file prefix")
    phase_structure: str = Field("hierarchical", description="Plan phase structure")
    
    @validator('phase_structure')
    def validate_phase_structure(cls, v):
        allowed = ['hierarchical', 'flat', 'component-based']
        if v not in allowed:
            raise ValueError(f"phase_structure must be one of {allowed}")
        return v


class DevelopmentConfig(BaseModel):
    """Development workflow configuration."""
    
    verbose_logging: bool = Field(False, description="Enable verbose logging")
    progress_reporting: bool = Field(True, description="Enable progress reporting")
    error_recovery: str = Field("prompt", description="Error recovery strategy")
    
    @validator('error_recovery')
    def validate_error_recovery(cls, v):
        allowed = ['prompt', 'auto-retry', 'fail-fast', 'ignore']
        if v not in allowed:
            raise ValueError(f"error_recovery must be one of {allowed}")
        return v


class WorkflowConfig(BaseModel):
    """Workflow automation configuration."""
    
    sync: SyncConfig = Field(default_factory=SyncConfig)
    planning: PlanningConfig = Field(default_factory=PlanningConfig)
    development: DevelopmentConfig = Field(default_factory=DevelopmentConfig)


class ComplianceConfig(BaseModel):
    """Compliance and safety configuration."""
    
    enforcement_level: str = Field("strict", description="Compliance enforcement level")
    audit_logging: bool = Field(True, description="Enable audit logging")
    content_filtering: bool = Field(True, description="Enable content filtering")
    credential_storage: str = Field("secure", description="Credential storage method")
    age_verification: bool = Field(True, description="Require age verification")
    retention_period: str = Field("30d", description="Data retention period")
    
    @validator('enforcement_level')
    def validate_enforcement_level(cls, v):
        allowed = ['strict', 'moderate', 'permissive']
        if v not in allowed:
            raise ValueError(f"enforcement_level must be one of {allowed}")
        return v
    
    @validator('credential_storage')
    def validate_credential_storage(cls, v):
        allowed = ['secure', 'encrypted', 'plaintext']
        if v not in allowed:
            raise ValueError(f"credential_storage must be one of {allowed}")
        return v


class SecurityConfig(BaseModel):
    """Security configuration."""
    
    allow_dangerous_operations: bool = Field(False, description="Allow dangerous operations")
    require_explicit_permissions: bool = Field(True, description="Require explicit permissions")
    network_isolation: bool = Field(True, description="Enable network isolation")
    sandboxed_execution: bool = Field(True, description="Enable sandboxed execution")


class ProjectConfig(BaseModel):
    """Project metadata configuration."""
    
    name: str = Field("FlowLoom", description="Project name")
    type: str = Field("ai-coordination-platform", description="Project type")
    description: Optional[str] = Field(None, description="Project description")
    version: Optional[str] = Field(None, description="Project version")


class EnvironmentConfig(BaseModel):
    """Environment and path configuration."""
    
    work_dir: str = Field("${FLOWLOOM_WORK_DIR:-.meta-claude}", description="Working directory")
    config_paths: Dict[str, str] = Field(
        default_factory=lambda: {
            "commands": ".claude/commands",
            "backup": "backup/.claude", 
            "plans": "plans"
        },
        description="Configuration paths"
    )


class FlowLoomConfig(BaseModel):
    """Complete FlowLoom configuration."""
    
    version: str = Field("1.0", description="Configuration version")
    project: ProjectConfig = Field(default_factory=ProjectConfig)
    agents: AgentConfig = Field(default_factory=AgentConfig)
    resources: ResourceConfig = Field(default_factory=ResourceConfig)
    workflow: WorkflowConfig = Field(default_factory=WorkflowConfig)
    compliance: ComplianceConfig = Field(default_factory=ComplianceConfig)
    security: SecurityConfig = Field(default_factory=SecurityConfig)
    environment: EnvironmentConfig = Field(default_factory=EnvironmentConfig)
    
    class Config:
        """Pydantic configuration."""
        extra = "forbid"  # Don't allow extra fields
        validate_assignment = True  # Validate on assignment
        use_enum_values = True  # Use enum values in serialization
    
    @validator('version')
    def validate_version(cls, v):
        # Validate semantic version format
        pattern = r'^\d+\.\d+(\.\d+)?$'
        if not re.match(pattern, v):
            raise ValueError("version must be in semantic version format (e.g., '1.0', '1.0.0')")
        return v
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization."""
        return self.dict(exclude_unset=False)
    
    def to_json(self, **kwargs) -> str:
        """Convert to JSON string."""
        return self.json(indent=2, **kwargs)
    
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'FlowLoomConfig':
        """Create from dictionary."""
        return cls(**data)
    
    @classmethod
    def create_default(cls) -> 'FlowLoomConfig':
        """Create default configuration."""
        return cls()
    
    def merge_with(self, other: 'FlowLoomConfig') -> 'FlowLoomConfig':
        """Merge with another configuration (other takes precedence)."""
        # Convert both to dicts
        base_dict = self.to_dict()
        other_dict = other.to_dict()
        
        # Deep merge
        merged_dict = self._deep_merge(base_dict, other_dict)
        
        return self.from_dict(merged_dict)
    
    def _deep_merge(self, base: Dict[str, Any], override: Dict[str, Any]) -> Dict[str, Any]:
        """Deep merge two dictionaries."""
        result = base.copy()
        
        for key, value in override.items():
            if key in result and isinstance(result[key], dict) and isinstance(value, dict):
                result[key] = self._deep_merge(result[key], value)
            else:
                result[key] = value
        
        return result