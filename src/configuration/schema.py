"""Configuration schema and validation system for FlowLoom.

This module provides JSON schema definitions and validation utilities
for FlowLoom configuration files and runtime configuration validation.
"""

import json
from pathlib import Path
from typing import Dict, Any, List, Optional, Union
from dataclasses import dataclass

from .models import FlowLoomConfig


@dataclass
class ValidationResult:
    """Result of configuration validation."""
    valid: bool
    errors: List[str]
    warnings: List[str]
    deprecated_keys: List[str]


class ConfigurationSchema:
    """Manages JSON schema for FlowLoom configuration."""
    
    def __init__(self):
        self.schema = self._build_json_schema()
    
    def _build_json_schema(self) -> Dict[str, Any]:
        """Build JSON schema from Pydantic models."""
        return {
            "$schema": "http://json-schema.org/draft-07/schema#",
            "title": "FlowLoom Configuration",
            "description": "Configuration schema for FlowLoom AI coordination platform",
            "type": "object",
            "properties": {
                "version": {
                    "type": "string",
                    "pattern": r"^\d+\.\d+(\.\d+)?$",
                    "description": "Configuration schema version"
                },
                "project": {
                    "type": "object",
                    "properties": {
                        "name": {"type": "string", "minLength": 1},
                        "description": {"type": "string"},
                        "type": {"type": "string"},
                        "repository": {"type": "string", "format": "uri"}
                    },
                    "required": ["name"],
                    "additionalProperties": False
                },
                "agents": {
                    "type": "object",
                    "properties": {
                        "maxConcurrent": {
                            "type": "integer",
                            "minimum": 1,
                            "maximum": 10,
                            "default": 3
                        },
                        "coordinationStrategy": {
                            "type": "string",
                            "enum": ["hierarchical", "peer-to-peer", "centralized"],
                            "default": "hierarchical"
                        },
                        "defaultModel": {
                            "type": "string",
                            "default": "claude-sonnet-4"
                        },
                        "memorySystem": {
                            "type": "string",
                            "enum": ["mcp-only", "basic-only", "dual"],
                            "default": "dual"
                        },
                        "agentTimeout": {
                            "type": "string",
                            "pattern": r"^\d+[smh]$",
                            "default": "30m"
                        },
                        "crossAgentSharing": {
                            "type": "boolean",
                            "default": True
                        }
                    },
                    "additionalProperties": False
                },
                "resources": {
                    "type": "object",
                    "properties": {
                        "rateLimiting": {
                            "type": "object",
                            "properties": {
                                "globalRequestsPerMinute": {
                                    "type": "integer",
                                    "minimum": 1,
                                    "maximum": 1000,
                                    "default": 100
                                },
                                "perAgentRequestsPerMinute": {
                                    "type": "integer", 
                                    "minimum": 1,
                                    "maximum": 100,
                                    "default": 30
                                },
                                "burstAllowance": {
                                    "type": "integer",
                                    "minimum": 1,
                                    "maximum": 50,
                                    "default": 10
                                }
                            },
                            "additionalProperties": False
                        },
                        "memory": {
                            "type": "object",
                            "properties": {
                                "retentionPeriod": {
                                    "type": "string",
                                    "pattern": r"^\d+[dwy]$",
                                    "default": "7d"
                                },
                                "maxContextWindow": {
                                    "type": "string",
                                    "pattern": r"^\d+k?$",
                                    "default": "200k"
                                },
                                "contextStrategy": {
                                    "type": "string",
                                    "enum": ["truncate-oldest", "truncate-middle", "summarize"],
                                    "default": "truncate-oldest"
                                }
                            },
                            "additionalProperties": False
                        },
                        "retryPolicy": {
                            "type": "object",
                            "properties": {
                                "maxRetries": {
                                    "type": "integer",
                                    "minimum": 0,
                                    "maximum": 10,
                                    "default": 3
                                },
                                "backoffStrategy": {
                                    "type": "string",
                                    "enum": ["fixed", "linear", "exponential"],
                                    "default": "exponential"
                                },
                                "baseDelay": {
                                    "type": "string",
                                    "pattern": r"^\d+(\.\d+)?[sm]$",
                                    "default": "1s"
                                }
                            },
                            "additionalProperties": False
                        }
                    },
                    "additionalProperties": False
                },
                "workflow": {
                    "type": "object",
                    "properties": {
                        "sync": {
                            "type": "object",
                            "properties": {
                                "strategy": {
                                    "type": "string",
                                    "enum": ["basic", "comprehensive", "minimal"],
                                    "default": "comprehensive"
                                },
                                "autoCommit": {"type": "boolean", "default": True},
                                "generateCommitMessages": {"type": "boolean", "default": True},
                                "pushOnSync": {"type": "boolean", "default": True},
                                "backupConfigs": {"type": "boolean", "default": True}
                            },
                            "additionalProperties": False
                        },
                        "planning": {
                            "type": "object",
                            "properties": {
                                "autoNumbering": {"type": "boolean", "default": True},
                                "planPrefix": {"type": "string", "default": "FlowLoom"},
                                "phaseStructure": {
                                    "type": "string",
                                    "enum": ["flat", "hierarchical", "hybrid"],
                                    "default": "hierarchical"
                                }
                            },
                            "additionalProperties": False
                        },
                        "development": {
                            "type": "object",
                            "properties": {
                                "verboseLogging": {"type": "boolean", "default": False},
                                "progressReporting": {"type": "boolean", "default": True},
                                "errorRecovery": {
                                    "type": "string",
                                    "enum": ["prompt", "retry", "abort"],
                                    "default": "prompt"
                                }
                            },
                            "additionalProperties": False
                        }
                    },
                    "additionalProperties": False
                },
                "compliance": {
                    "type": "object",
                    "properties": {
                        "enforcementLevel": {
                            "type": "string",
                            "enum": ["strict", "moderate", "lenient"],
                            "default": "strict"
                        },
                        "auditLogging": {"type": "boolean", "default": True},
                        "contentFiltering": {"type": "boolean", "default": True},
                        "credentialStorage": {
                            "type": "string",
                            "enum": ["secure", "basic", "none"],
                            "default": "secure"
                        },
                        "ageVerification": {"type": "boolean", "default": True},
                        "retentionPeriod": {
                            "type": "string",
                            "pattern": r"^\d+[dwy]$",
                            "default": "30d"
                        }
                    },
                    "additionalProperties": False
                },
                "security": {
                    "type": "object",
                    "properties": {
                        "allowDangerousOperations": {"type": "boolean", "default": False},
                        "requireExplicitPermissions": {"type": "boolean", "default": True},
                        "networkIsolation": {"type": "boolean", "default": True},
                        "sandboxedExecution": {"type": "boolean", "default": True}
                    },
                    "additionalProperties": False
                },
                "environment": {
                    "type": "object",
                    "properties": {
                        "workDir": {
                            "type": "string",
                            "default": "${FLOWLOOM_WORK_DIR:-.meta-claude}"
                        },
                        "configPaths": {
                            "type": "object",
                            "properties": {
                                "commands": {"type": "string", "default": ".claude/commands"},
                                "backup": {"type": "string", "default": "backup/.claude"},
                                "plans": {"type": "string", "default": "plans"}
                            },
                            "additionalProperties": False
                        }
                    },
                    "additionalProperties": False
                }
            },
            "required": ["version"],
            "additionalProperties": False
        }
    
    def validate_json(self, config_data: Dict[str, Any]) -> ValidationResult:
        """Validate configuration data against JSON schema."""
        import jsonschema
        from jsonschema import validate, ValidationError, Draft7Validator
        
        errors = []
        warnings = []
        deprecated_keys = []
        
        try:
            # Basic schema validation
            validate(instance=config_data, schema=self.schema)
            
            # Custom validation rules
            custom_errors = self._validate_custom_rules(config_data)
            errors.extend(custom_errors)
            
            # Check for deprecated keys
            deprecated = self._check_deprecated_keys(config_data)
            deprecated_keys.extend(deprecated)
            if deprecated:
                warnings.extend([f"Deprecated key: {key}" for key in deprecated])
            
            # Business logic validation
            business_warnings = self._validate_business_logic(config_data)
            warnings.extend(business_warnings)
            
        except ValidationError as e:
            errors.append(f"Schema validation error: {e.message}")
        except Exception as e:
            errors.append(f"Validation error: {str(e)}")
        
        return ValidationResult(
            valid=len(errors) == 0,
            errors=errors,
            warnings=warnings,
            deprecated_keys=deprecated_keys
        )
    
    def _validate_custom_rules(self, config_data: Dict[str, Any]) -> List[str]:
        """Apply custom validation rules beyond JSON schema."""
        errors = []
        
        # Version validation
        version = config_data.get("version")
        if version and not self._is_supported_version(version):
            errors.append(f"Unsupported configuration version: {version}")
        
        # Cross-field validation
        agents = config_data.get("agents", {})
        resources = config_data.get("resources", {})
        
        # Validate agent timeout vs rate limiting
        if agents.get("agentTimeout") and resources.get("rateLimiting"):
            timeout_seconds = self._parse_timeout(agents["agentTimeout"])
            rate_limit = resources["rateLimiting"].get("perAgentRequestsPerMinute", 30)
            
            if timeout_seconds > 0 and rate_limit > 0:
                max_requests = (timeout_seconds / 60) * rate_limit
                if max_requests > 1000:  # Arbitrary threshold
                    errors.append(
                        f"Agent timeout ({agents['agentTimeout']}) and rate limit "
                        f"({rate_limit}/min) combination may exceed API limits"
                    )
        
        # Validate memory settings
        memory = resources.get("memory", {})
        if memory.get("maxContextWindow"):
            context_size = self._parse_context_window(memory["maxContextWindow"])
            if context_size > 1000000:  # 1M tokens
                errors.append(
                    f"Context window size {memory['maxContextWindow']} may be too large"
                )
        
        return errors
    
    def _check_deprecated_keys(self, config_data: Dict[str, Any]) -> List[str]:
        """Check for deprecated configuration keys."""
        deprecated_keys = []
        
        # Define deprecated key mappings
        deprecated_mappings = {
            "agentConfig": "agents",
            "resourceConfig": "resources", 
            "workflowConfig": "workflow",
            "maxAgents": "agents.maxConcurrent",
            "coordination": "agents.coordinationStrategy"
        }
        
        def check_deprecated_recursive(data: Dict[str, Any], path: str = ""):
            for key, value in data.items():
                current_path = f"{path}.{key}" if path else key
                
                if key in deprecated_mappings:
                    deprecated_keys.append(current_path)
                
                if isinstance(value, dict):
                    check_deprecated_recursive(value, current_path)
        
        check_deprecated_recursive(config_data)
        return deprecated_keys
    
    def _validate_business_logic(self, config_data: Dict[str, Any]) -> List[str]:
        """Validate business logic constraints."""
        warnings = []
        
        # Check for overly permissive security settings
        security = config_data.get("security", {})
        if security.get("allowDangerousOperations") and security.get("networkIsolation") is False:
            warnings.append(
                "Dangerous operations enabled with network isolation disabled - "
                "consider enabling network isolation for security"
            )
        
        # Check coordination strategy vs agent count
        agents = config_data.get("agents", {})
        max_concurrent = agents.get("maxConcurrent", 3)
        strategy = agents.get("coordinationStrategy", "hierarchical")
        
        if strategy == "peer-to-peer" and max_concurrent > 5:
            warnings.append(
                f"Peer-to-peer coordination with {max_concurrent} agents may be complex - "
                "consider hierarchical coordination for better management"
            )
        
        # Check memory retention vs compliance requirements
        resources = config_data.get("resources", {})
        compliance = config_data.get("compliance", {})
        
        memory_retention = resources.get("memory", {}).get("retentionPeriod", "7d")
        compliance_retention = compliance.get("retentionPeriod", "30d")
        
        memory_days = self._parse_retention_period(memory_retention)
        compliance_days = self._parse_retention_period(compliance_retention)
        
        if memory_days > compliance_days:
            warnings.append(
                f"Memory retention ({memory_retention}) exceeds compliance requirement "
                f"({compliance_retention}) - may violate data retention policies"
            )
        
        return warnings
    
    def _is_supported_version(self, version: str) -> bool:
        """Check if configuration version is supported."""
        supported_versions = ["1.0", "1.1"]
        return version in supported_versions
    
    def _parse_timeout(self, timeout_str: str) -> int:
        """Parse timeout string to seconds."""
        import re
        match = re.match(r'^(\d+)([smh])$', timeout_str)
        if not match:
            return 0
        
        value, unit = match.groups()
        multipliers = {'s': 1, 'm': 60, 'h': 3600}
        return int(value) * multipliers.get(unit, 0)
    
    def _parse_context_window(self, context_str: str) -> int:
        """Parse context window string to token count."""
        import re
        match = re.match(r'^(\d+)k?$', context_str)
        if not match:
            return 0
        
        value = int(match.group(1))
        if context_str.endswith('k'):
            value *= 1000
        return value
    
    def _parse_retention_period(self, period_str: str) -> int:
        """Parse retention period string to days."""
        import re
        match = re.match(r'^(\d+)([dwy])$', period_str)
        if not match:
            return 0
        
        value, unit = match.groups()
        multipliers = {'d': 1, 'w': 7, 'y': 365}
        return int(value) * multipliers.get(unit, 0)
    
    def get_schema_json(self) -> str:
        """Get JSON schema as formatted string."""
        return json.dumps(self.schema, indent=2)
    
    def save_schema(self, path: Path) -> None:
        """Save JSON schema to file."""
        with open(path, 'w') as f:
            json.dump(self.schema, f, indent=2)


class ConfigurationValidator:
    """High-level configuration validation interface."""
    
    def __init__(self):
        self.schema = ConfigurationSchema()
    
    def validate_config(self, config: Union[FlowLoomConfig, Dict[str, Any], str, Path]) -> ValidationResult:
        """Validate configuration from various input types."""
        if isinstance(config, FlowLoomConfig):
            # Convert Pydantic model to dict for JSON schema validation
            config_data = config.dict()
        elif isinstance(config, dict):
            config_data = config
        elif isinstance(config, (str, Path)):
            # Load from file
            config_path = Path(config)
            if not config_path.exists():
                return ValidationResult(
                    valid=False,
                    errors=[f"Configuration file not found: {config_path}"],
                    warnings=[],
                    deprecated_keys=[]
                )
            
            try:
                with open(config_path) as f:
                    config_data = json.load(f)
            except json.JSONDecodeError as e:
                return ValidationResult(
                    valid=False,
                    errors=[f"Invalid JSON in configuration file: {e}"],
                    warnings=[],
                    deprecated_keys=[]
                )
            except Exception as e:
                return ValidationResult(
                    valid=False,
                    errors=[f"Error reading configuration file: {e}"],
                    warnings=[],
                    deprecated_keys=[]
                )
        else:
            return ValidationResult(
                valid=False,
                errors=[f"Unsupported configuration type: {type(config)}"],
                warnings=[],
                deprecated_keys=[]
            )
        
        return self.schema.validate_json(config_data)
    
    def validate_and_upgrade(self, config_data: Dict[str, Any]) -> tuple[Dict[str, Any], ValidationResult]:
        """Validate configuration and apply automatic upgrades for deprecated keys."""
        result = self.schema.validate_json(config_data)
        upgraded_config = self._apply_upgrades(config_data, result.deprecated_keys)
        
        return upgraded_config, result
    
    def _apply_upgrades(self, config_data: Dict[str, Any], deprecated_keys: List[str]) -> Dict[str, Any]:
        """Apply automatic upgrades for deprecated configuration keys."""
        upgraded = config_data.copy()
        
        # Define upgrade mappings
        upgrade_mappings = {
            "agentConfig": "agents",
            "resourceConfig": "resources",
            "workflowConfig": "workflow",
            "maxAgents": ("agents", "maxConcurrent"),
            "coordination": ("agents", "coordinationStrategy")
        }
        
        for deprecated_key in deprecated_keys:
            if deprecated_key in upgrade_mappings:
                mapping = upgrade_mappings[deprecated_key]
                
                if isinstance(mapping, str):
                    # Simple key rename
                    if deprecated_key in upgraded:
                        upgraded[mapping] = upgraded.pop(deprecated_key)
                elif isinstance(mapping, tuple):
                    # Move to nested structure
                    section, key = mapping
                    if deprecated_key in upgraded:
                        if section not in upgraded:
                            upgraded[section] = {}
                        upgraded[section][key] = upgraded.pop(deprecated_key)
        
        return upgraded


# Export main classes
__all__ = [
    'ConfigurationSchema',
    'ConfigurationValidator', 
    'ValidationResult'
]