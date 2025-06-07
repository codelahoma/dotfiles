#!/usr/bin/env node

/**
 * FlowLoom Memory Entity Resolution System
 * Prevents duplicate entity creation through intelligent name matching
 */

const fs = require('fs');
const path = require('path');

class EntityResolver {
  constructor(memoryFilePath = './memory.json') {
    this.memoryFilePath = memoryFilePath;
    this.entities = this.loadEntities();
  }

  loadEntities() {
    try {
      const content = fs.readFileSync(this.memoryFilePath, 'utf8');
      return content.split('\n')
        .filter(line => line.trim())
        .map(line => JSON.parse(line))
        .filter(obj => obj.type === 'entity');
    } catch (error) {
      console.error('Error loading entities:', error.message);
      return [];
    }
  }

  /**
   * Generate canonical name from user input
   */
  generateCanonicalName(userInput, entityType) {
    // Remove common prefixes/suffixes
    let cleaned = userInput
      .replace(/^(the|a|an)\s+/i, '')
      .replace(/\s+(system|component|feature|service)$/i, '');

    // Extract scope
    let scope = 'FlowLoom';
    if (cleaned.toLowerCase().includes('project')) {
      scope = 'Project';
      cleaned = cleaned.replace(/project\s*/i, '');
    } else if (cleaned.toLowerCase().includes('session')) {
      scope = 'Session';
      cleaned = cleaned.replace(/session\s*/i, '');
    }

    // Extract component category
    const components = this.categorizeComponent(cleaned);
    
    // Build canonical name
    const parts = [scope, ...components, entityType];
    return parts
      .map(part => this.toPascalCase(part))
      .join(':');
  }

  categorizeComponent(input) {
    const categories = {
      'Core': ['command', 'memory', 'config', 'plan'],
      'Infrastructure': ['docker', 'install', 'session', 'git'],
      'Features': ['documentation', 'workflow', 'coordination'],
      'Integration': ['github', 'mcp', 'claude', 'api'],
      'Documentation': ['guide', 'spec', 'readme', 'plan']
    };

    const inputLower = input.toLowerCase();
    
    for (const [category, keywords] of Object.entries(categories)) {
      if (keywords.some(keyword => inputLower.includes(keyword))) {
        // Extract the specific component name
        const component = this.extractComponentName(input, keywords);
        return [category, component];
      }
    }

    // Default to Features if no category match
    return ['Features', this.toPascalCase(input)];
  }

  extractComponentName(input, matchedKeywords) {
    const keyword = matchedKeywords.find(k => input.toLowerCase().includes(k));
    const words = input.toLowerCase().split(/\s+/);
    
    // Find related words around the keyword
    const keywordIndex = words.findIndex(w => w.includes(keyword));
    const relevantWords = [];
    
    // Include the keyword and surrounding context
    for (let i = Math.max(0, keywordIndex - 1); i <= Math.min(words.length - 1, keywordIndex + 1); i++) {
      relevantWords.push(words[i]);
    }
    
    return this.toPascalCase(relevantWords.join(' '));
  }

  toPascalCase(str) {
    return str
      .split(/[\s\-_]+/)
      .map(word => word.charAt(0).toUpperCase() + word.slice(1).toLowerCase())
      .join('');
  }

  /**
   * Find existing entities that might match the input
   */
  findMatches(canonicalName, userInput) {
    const matches = [];

    for (const entity of this.entities) {
      const confidence = this.calculateMatchConfidence(entity, canonicalName, userInput);
      if (confidence > 0.6) {
        matches.push({ entity, confidence });
      }
    }

    // Sort by confidence (highest first)
    return matches.sort((a, b) => b.confidence - a.confidence);
  }

  calculateMatchConfidence(entity, canonicalName, userInput) {
    let confidence = 0;

    // Exact canonical match
    if (entity.name === canonicalName) {
      return 1.0;
    }

    // Case-insensitive canonical match
    if (entity.name.toLowerCase() === canonicalName.toLowerCase()) {
      confidence = Math.max(confidence, 0.9);
    }

    // Check aliases (if supported)
    if (entity.aliases && entity.aliases.includes(userInput)) {
      confidence = Math.max(confidence, 0.8);
    }

    // Keyword overlap in name
    const nameWords = entity.name.toLowerCase().split(/[:\s\-_]+/);
    const inputWords = userInput.toLowerCase().split(/\s+/);
    const nameOverlap = this.calculateOverlap(nameWords, inputWords);
    confidence = Math.max(confidence, nameOverlap * 0.7);

    // Keyword overlap in observations
    const observations = entity.observations.join(' ').toLowerCase();
    const obsOverlap = inputWords.filter(word => observations.includes(word)).length / inputWords.length;
    confidence = Math.max(confidence, obsOverlap * 0.6);

    return confidence;
  }

  calculateOverlap(arr1, arr2) {
    const intersection = arr1.filter(item => arr2.includes(item));
    return intersection.length / Math.max(arr1.length, arr2.length);
  }

  /**
   * Main resolution function
   */
  resolveEntity(userInput, entityType) {
    const canonicalName = this.generateCanonicalName(userInput, entityType);
    const matches = this.findMatches(canonicalName, userInput);

    return {
      canonicalName,
      suggestedName: canonicalName,
      existingMatches: matches,
      shouldCreate: matches.length === 0 || matches[0].confidence < 0.8,
      recommendation: this.generateRecommendation(matches, canonicalName)
    };
  }

  generateRecommendation(matches, canonicalName) {
    if (matches.length === 0) {
      return `Create new entity: "${canonicalName}"`;
    }

    const bestMatch = matches[0];
    if (bestMatch.confidence >= 0.9) {
      return `Use existing entity: "${bestMatch.entity.name}" (${Math.round(bestMatch.confidence * 100)}% match)`;
    } else if (bestMatch.confidence >= 0.7) {
      return `Possible duplicate of: "${bestMatch.entity.name}" (${Math.round(bestMatch.confidence * 100)}% match). Consider merging or use different name.`;
    } else {
      return `Create new entity: "${canonicalName}". Similar entities found but low confidence.`;
    }
  }
}

// CLI interface
if (require.main === module) {
  const args = process.argv.slice(2);
  if (args.length < 2) {
    console.log('Usage: memory-entity-resolver.js <entity_name> <entity_type> [memory_file]');
    console.log('Example: memory-entity-resolver.js "docker environment" "Component"');
    process.exit(1);
  }

  const [entityName, entityType, memoryFile] = args;
  const resolver = new EntityResolver(memoryFile);
  const result = resolver.resolveEntity(entityName, entityType);

  console.log('Entity Resolution Result:');
  console.log('========================');
  console.log(`Input: "${entityName}" (${entityType})`);
  console.log(`Canonical Name: ${result.canonicalName}`);
  console.log(`Recommendation: ${result.recommendation}`);
  
  if (result.existingMatches.length > 0) {
    console.log('\nExisting Matches:');
    result.existingMatches.slice(0, 3).forEach((match, i) => {
      console.log(`  ${i + 1}. ${match.entity.name} (${Math.round(match.confidence * 100)}% confidence)`);
    });
  }
}

module.exports = EntityResolver;