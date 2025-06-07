"""
CLI Interface for FlowLoom Memory Monitor.

Provides command-line access to memory monitoring, querying,
and streaming capabilities.
"""

import asyncio
import json
import sys
import time
from pathlib import Path
from typing import Optional, Dict, Any
import logging

import click

from .monitor import MemoryMonitor, MonitorEvent
from .query import MemoryQuery, QueryError
from .file_observer import FileObserver

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger(__name__)


def setup_logging(verbose: bool):
    """Set up logging level based on verbosity."""
    if verbose:
        logging.getLogger('memory_monitor').setLevel(logging.DEBUG)
    else:
        logging.getLogger('memory_monitor').setLevel(logging.WARNING)


@click.group()
@click.option('--verbose', '-v', is_flag=True, help='Enable verbose logging')
@click.pass_context
def cli(ctx, verbose):
    """FlowLoom Memory Monitor CLI."""
    ctx.ensure_object(dict)
    ctx.obj['verbose'] = verbose
    setup_logging(verbose)


@cli.command()
@click.argument('memory_file', type=click.Path(exists=True))
@click.argument('sql_query')
@click.option('--output', '-o', type=click.Choice(['json', 'table', 'csv']), default='table')
@click.option('--page', type=int, default=1, help='Page number for pagination')
@click.option('--page-size', type=int, default=20, help='Number of results per page')
@click.option('--count-only', is_flag=True, help='Show only result count')
@click.pass_context
def query(ctx, memory_file, sql_query, output, page, page_size, count_only):
    """Execute SQL query against memory data."""
    try:
        query_interface = MemoryQuery(memory_file)
        result = query_interface.query(sql_query)
        
        if count_only:
            click.echo(result.count())
            return
        
        if output == 'json':
            if page == 1 and page_size >= result.count():
                # Return all results
                data = {
                    'query': sql_query,
                    'results': result.fetch_all(),
                    'count': result.count(),
                    'stats': result.get_stats().__dict__
                }
            else:
                # Return paginated results
                data = result.fetch_page(page, page_size)
            
            click.echo(json.dumps(data, indent=2, default=str))
        
        elif output == 'csv':
            results = result.fetch_page(page, page_size)['items'] if page > 1 or page_size < result.count() else result.fetch_all()
            
            if results:
                # Get all possible columns
                columns = set()
                for item in results:
                    columns.update(item.keys())
                columns = sorted(columns)
                
                # Print header
                click.echo(','.join(columns))
                
                # Print rows
                for item in results:
                    row = []
                    for col in columns:
                        value = item.get(col, '')
                        if isinstance(value, list):
                            value = ';'.join(str(v) for v in value)
                        elif isinstance(value, dict):
                            value = json.dumps(value)
                        row.append(f'"{value}"')
                    click.echo(','.join(row))
        
        else:  # table format
            if page > 1 or page_size < result.count():
                page_data = result.fetch_page(page, page_size)
                results = page_data['items']
                
                click.echo(f"Results: {page_data['total']} total, showing page {page_data['page']} of {page_data['total_pages']}")
                click.echo(f"Query: {sql_query}")
                click.echo("-" * 80)
            else:
                results = result.fetch_all()
                click.echo(f"Results: {result.count()} total")
                click.echo(f"Query: {sql_query}")
                click.echo("-" * 80)
            
            if not results:
                click.echo("No results found.")
                return
            
            # Print table
            for i, item in enumerate(results, 1):
                click.echo(f"{i}. {item.get('name', 'Unknown')} ({item.get('entityType', 'Unknown')})")
                
                # Show observations if present
                if 'observations' in item and item['observations']:
                    for obs in item['observations'][:3]:  # Show first 3
                        click.echo(f"   • {obs}")
                    if len(item['observations']) > 3:
                        click.echo(f"   ... and {len(item['observations']) - 3} more")
                
                if i < len(results):
                    click.echo()
            
            # Show stats
            stats = result.get_stats()
            click.echo(f"\nExecution: {stats.execution_time_ms:.2f}ms, scanned {stats.entities_scanned} entities")
    
    except QueryError as e:
        click.echo(f"Query error: {e}", err=True)
        sys.exit(1)
    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)


@cli.command()
@click.argument('memory_file', type=click.Path(exists=True))
@click.argument('sql_query')
@click.pass_context
def explain(ctx, memory_file, sql_query):
    """Explain query execution plan."""
    try:
        query_interface = MemoryQuery(memory_file)
        plan = query_interface.explain(sql_query)
        
        click.echo("Query Execution Plan")
        click.echo("=" * 50)
        click.echo(f"SQL: {plan['sql']}")
        click.echo()
        
        click.echo("AST:")
        ast = plan['ast']
        click.echo(f"  SELECT: {', '.join(ast['select'])}")
        click.echo(f"  FROM: {ast['from']}")
        if ast['where']:
            click.echo(f"  WHERE: {ast['where']}")
        if ast['order_by']:
            click.echo(f"  ORDER BY: {ast['order_by']}")
        if ast['limit']:
            click.echo(f"  LIMIT: {ast['limit']}")
        
        click.echo()
        click.echo(f"Available indexes: {', '.join(plan['indexes_available'])}")
        click.echo(f"Entity count: {plan['entity_count']}")
    
    except QueryError as e:
        click.echo(f"Query error: {e}", err=True)
        sys.exit(1)


@cli.command()
@click.argument('memory_file', type=click.Path(exists=True))
@click.option('--timeout', '-t', type=float, help='Timeout in seconds')
@click.option('--poll-interval', type=float, default=1.0, help='Polling interval in seconds')
@click.pass_context
def watch(ctx, memory_file, timeout, poll_interval):
    """Watch memory file for changes."""
    async def watch_changes():
        monitor = MemoryMonitor(memory_file, poll_interval=poll_interval)
        
        def handle_event(event: MonitorEvent):
            timestamp = time.strftime('%H:%M:%S', time.localtime(event.timestamp))
            click.echo(f"[{timestamp}] {event.event_type}")
            
            if event.event_type == 'entity_created':
                entity = event.data['entity']
                click.echo(f"  Created: {entity.get('name', 'Unknown')} ({entity.get('entityType', 'Unknown')})")
            
            elif event.event_type == 'entity_updated':
                entity = event.data['entity']
                changes = event.data['changes']
                click.echo(f"  Updated: {entity.get('name', 'Unknown')}")
                for field, change in changes.items():
                    if field == 'observations':
                        if change['added']:
                            click.echo(f"    + {', '.join(change['added'])}")
                        if change['removed']:
                            click.echo(f"    - {', '.join(change['removed'])}")
                    else:
                        click.echo(f"    {field}: {change['old']} → {change['new']}")
            
            elif event.event_type == 'entity_deleted':
                entity = event.data['entity']
                click.echo(f"  Deleted: {entity.get('name', 'Unknown')}")
        
        monitor.add_event_handler(handle_event)
        
        click.echo(f"Watching {memory_file} for changes... (Ctrl+C to stop)")
        
        try:
            if timeout:
                await asyncio.wait_for(monitor.start_monitoring(), timeout=timeout)
            else:
                await monitor.start_monitoring()
        except asyncio.TimeoutError:
            click.echo("Watch timeout reached")
        except KeyboardInterrupt:
            click.echo("\nStopping watch...")
        finally:
            monitor.stop_monitoring()
    
    try:
        asyncio.run(watch_changes())
    except KeyboardInterrupt:
        pass


@cli.command()
@click.argument('memory_file', type=click.Path(exists=True))
@click.argument('sql_query')
@click.option('--timeout', '-t', type=float, default=300.0, help='Timeout in seconds')
@click.option('--output', '-o', type=click.Choice(['json', 'text']), default='text')
@click.pass_context
def wait_for(ctx, memory_file, sql_query, timeout, output):
    """Wait for query condition to be met."""
    async def wait_for_condition():
        monitor = MemoryMonitor(memory_file)
        
        click.echo(f"Waiting for condition: {sql_query}")
        click.echo(f"Timeout: {timeout}s")
        
        start_time = time.time()
        result = await monitor.wait_for_condition(sql_query, timeout)
        elapsed = time.time() - start_time
        
        if result:
            if output == 'json':
                data = result.to_dict()
                click.echo(json.dumps(data, indent=2, default=str))
            else:
                click.echo(f"Condition met after {elapsed:.2f}s")
                items = result.fetch_all()
                click.echo(f"Found {len(items)} matching entities:")
                for item in items:
                    click.echo(f"  • {item.get('name', 'Unknown')} ({item.get('entityType', 'Unknown')})")
        else:
            click.echo(f"Condition not met within {timeout}s")
            sys.exit(1)
    
    try:
        asyncio.run(wait_for_condition())
    except KeyboardInterrupt:
        click.echo("\nWait cancelled")
        sys.exit(1)


@cli.command()
@click.argument('memory_file', type=click.Path(exists=True))
@click.option('--output', '-o', type=click.Choice(['json', 'text']), default='text')
@click.pass_context
def stats(ctx, memory_file, output):
    """Show memory file statistics."""
    try:
        query_interface = MemoryQuery(memory_file)
        stats_data = query_interface.get_stats()
        
        if output == 'json':
            click.echo(json.dumps(stats_data, indent=2, default=str))
        else:
            click.echo("Memory File Statistics")
            click.echo("=" * 30)
            click.echo(f"File: {stats_data['file_path']}")
            click.echo(f"Entities: {stats_data['entity_count']}")
            click.echo(f"Relations: {stats_data['relation_count']}")
            
            click.echo(f"\nEntity Types:")
            for entity_type, count in stats_data['entity_types'].items():
                click.echo(f"  {entity_type}: {count}")
            
            if stats_data['relation_types']:
                click.echo(f"\nRelation Types:")
                for relation_type, count in stats_data['relation_types'].items():
                    click.echo(f"  {relation_type}: {count}")
            
            click.echo(f"\nIndexes: {', '.join(stats_data['indexes'])}")
    
    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)


@cli.command()
@click.argument('memory_file', type=click.Path(exists=True))
@click.option('--port', '-p', type=int, default=8765, help='WebSocket port')
@click.option('--host', '-h', default='localhost', help='Host to bind to')
@click.pass_context
def serve(ctx, memory_file, port, host):
    """Start WebSocket server for real-time updates."""
    try:
        import websockets
    except ImportError:
        click.echo("WebSocket support requires 'websockets' package: pip install websockets", err=True)
        sys.exit(1)
    
    async def handle_client(websocket, path):
        """Handle WebSocket client connection."""
        client_id = f"{websocket.remote_address[0]}:{websocket.remote_address[1]}"
        click.echo(f"Client connected: {client_id}")
        
        monitor = MemoryMonitor(memory_file)
        
        def send_event(event: MonitorEvent):
            """Send event to client."""
            try:
                message = {
                    'type': event.event_type,
                    'timestamp': event.timestamp,
                    'data': event.data
                }
                asyncio.create_task(websocket.send(json.dumps(message, default=str)))
            except Exception as e:
                logger.error(f"Error sending to client {client_id}: {e}")
        
        monitor.add_event_handler(send_event)
        
        try:
            # Send initial state
            stats = monitor.query_interface.get_stats()
            initial_message = {
                'type': 'initial_state',
                'timestamp': time.time(),
                'data': stats
            }
            await websocket.send(json.dumps(initial_message, default=str))
            
            # Start monitoring in background
            monitor_task = asyncio.create_task(monitor.start_monitoring())
            
            # Keep connection alive
            async for message in websocket:
                try:
                    data = json.loads(message)
                    
                    if data.get('type') == 'query':
                        # Handle query request
                        sql = data.get('sql', '')
                        try:
                            result = monitor.query(sql)
                            response = {
                                'type': 'query_result',
                                'timestamp': time.time(),
                                'data': {
                                    'sql': sql,
                                    'results': result.fetch_all()[:100],  # Limit results
                                    'count': result.count()
                                }
                            }
                            await websocket.send(json.dumps(response, default=str))
                        except QueryError as e:
                            error_response = {
                                'type': 'query_error',
                                'timestamp': time.time(),
                                'data': {'error': str(e), 'sql': sql}
                            }
                            await websocket.send(json.dumps(error_response, default=str))
                
                except json.JSONDecodeError:
                    pass  # Ignore invalid JSON
                
        except websockets.exceptions.ConnectionClosed:
            pass
        finally:
            click.echo(f"Client disconnected: {client_id}")
            monitor.stop_monitoring()
            if not monitor_task.done():
                monitor_task.cancel()
    
    async def start_server():
        """Start WebSocket server."""
        click.echo(f"Starting WebSocket server on {host}:{port}")
        click.echo(f"Monitoring: {memory_file}")
        
        async with websockets.serve(handle_client, host, port):
            click.echo("Server started. Press Ctrl+C to stop.")
            await asyncio.Future()  # Run forever
    
    try:
        asyncio.run(start_server())
    except KeyboardInterrupt:
        click.echo("\nServer stopped")


@cli.command()
@click.argument('memory_file', type=click.Path(exists=True))
@click.option('--entity-type', help='Filter by entity type')
@click.option('--search', help='Search in observations')
@click.option('--limit', type=int, default=10, help='Limit results')
@click.pass_context
def search(ctx, memory_file, entity_type, search, limit):
    """Quick search for entities."""
    conditions = []
    
    if entity_type:
        conditions.append(f"entityType = '{entity_type}'")
    
    if search:
        conditions.append(f"observations CONTAINS '{search}'")
    
    if not conditions:
        click.echo("Specify --entity-type or --search", err=True)
        sys.exit(1)
    
    where_clause = " AND ".join(conditions)
    sql = f"SELECT * FROM entities WHERE {where_clause} LIMIT {limit}"
    
    # Use existing query command
    ctx.invoke(query, memory_file=memory_file, sql_query=sql, output='table', page=1, page_size=limit, count_only=False)


if __name__ == '__main__':
    cli()