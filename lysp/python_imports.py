from __future__ import annotations
import importlib
import sys
from typing import Any, Dict, List, Optional, Tuple, Union
from .lreader import Syn, Symbol, SrcSpan
from .symbol_table import add_symbol, get_symbol, has_symbol, list_symbols

class ImportError(Exception):
    pass

class PythonImporter:
    """Handles Python imports for Lisp programs"""
    
    def __init__(self):
        self.imported_modules: Dict[str, Any] = {}
        self.imported_entities: Dict[str, Any] = {}
        self.import_aliases: Dict[str, str] = {}
    
    def import_module(self, module_name: str, alias: Optional[str] = None) -> Any:
        """Import a full Python module"""
        try:
            if module_name in self.imported_modules:
                module = self.imported_modules[module_name]
            else:
                module = importlib.import_module(module_name)
                self.imported_modules[module_name] = module
            
            if alias:
                self.import_aliases[alias] = module_name
                return module
            else:
                return module
        except ImportError as e:
            raise ImportError(f"Could not import module '{module_name}': {e}")
    
    def import_from(self, module_name: str, entity_names: List[str], 
                   aliases: Optional[List[str]] = None) -> Dict[str, Any]:
        """Import specific entities from a Python module"""
        try:
            if module_name in self.imported_modules:
                module = self.imported_modules[module_name]
            else:
                module = importlib.import_module(module_name)
                self.imported_modules[module_name] = module
            
            imported_entities = {}
            
            for i, entity_name in enumerate(entity_names):
                if not hasattr(module, entity_name):
                    raise ImportError(f"Module '{module_name}' has no attribute '{entity_name}'")
                
                entity = getattr(module, entity_name)
                alias = aliases[i] if aliases and i < len(aliases) else entity_name
                
                imported_entities[alias] = entity
                self.imported_entities[alias] = entity
                if alias != entity_name:
                    self.import_aliases[alias] = entity_name
            
            return imported_entities
        except ImportError as e:
            raise ImportError(f"Could not import from module '{module_name}': {e}")
    
    def get_imported_entity(self, name: str) -> Optional[Any]:
        """Get an imported entity by name"""
        return self.imported_entities.get(name)
    
    def get_module_by_alias(self, alias: str) -> Optional[Any]:
        """Get a module by its alias"""
        module_name = self.import_aliases.get(alias)
        if module_name:
            return self.imported_modules.get(module_name)
        return None
    
    def list_imported_modules(self) -> List[str]:
        """List all imported module names"""
        return list(self.imported_modules.keys())
    
    def list_imported_entities(self) -> List[str]:
        """List all imported entity names"""
        return list(self.imported_entities.keys())

# Global Python importer instance
python_importer = PythonImporter()

def import_python_module(module_name: str, alias: Optional[str] = None, import_all: bool = False) -> Any:
    """Import a Python module into Lisp
    If import_all is True, also bind all public attributes into the global symbol table by their bare names.
    Special-case: module_name == "python" creates a pseudo-module of Python built-in functions.
    """
    # Special handling for the pseudo-module 'python'
    if module_name == "python":
        import builtins
        from types import ModuleType
        module = ModuleType("python")
        # Expose only public callables from builtins
        for name in dir(builtins):
            if name.startswith('_'):
                continue
            try:
                val = getattr(builtins, name)
            except AttributeError:
                continue
            if callable(val):
                setattr(module, name, val)
        symbol_name = alias if alias else "python"
        add_symbol(symbol_name, module)
        for attr_name in dir(module):
            if not attr_name.startswith('_'):
                try:
                    attr_value = getattr(module, attr_name)
                    add_symbol(f"{symbol_name}.{attr_name}", attr_value)
                    if import_all:
                        add_symbol(attr_name, attr_value)
                except Exception:
                    pass
        # Also register in sys.modules so Python-side imports see it if needed
        sys.modules[symbol_name] = module
        return module

    # Standard module import path
    module = python_importer.import_module(module_name, alias)
    
    # Add to centralized symbol table
    symbol_name = alias if alias else module_name
    add_symbol(symbol_name, module)
    
    # Recursively add all objects from the module to the symbol table with qualified names
    for attr_name in dir(module):
        if not attr_name.startswith('_'):
            try:
                attr_value = getattr(module, attr_name)
                full_name = f"{symbol_name}.{attr_name}"
                add_symbol(full_name, attr_value)
                # Optionally add bare names
                if import_all:
                    add_symbol(attr_name, attr_value)
            except (AttributeError, TypeError):
                pass
    
    return module

def import_python_from(module_name: str, entity_names: List[str], 
                       aliases: Optional[List[str]] = None) -> Dict[str, Any]:
    """Import specific entities from a Python module"""
    imported_entities = python_importer.import_from(module_name, entity_names, aliases)
    
    # Add to centralized symbol table
    for alias, entity in imported_entities.items():
        add_symbol(alias, entity)
    
    return imported_entities

def get_python_entity(name: str) -> Optional[Any]:
    """Get an imported Python entity by name"""
    return python_importer.get_imported_entity(name)

def get_python_module(alias: str) -> Optional[Any]:
    """Get a Python module by alias"""
    return python_importer.get_module_by_alias(alias)

def list_python_imports() -> Tuple[List[str], List[str]]:
    """List all imported Python modules and entities"""
    return (python_importer.list_imported_modules(), 
            python_importer.list_imported_entities())

# Lisp to Python export functionality
class LispExporter:
    """Handles exporting Lisp functions and data to Python"""
    
    def __init__(self):
        self.exported_functions: Dict[str, Any] = {}
        self.exported_data: Dict[str, Any] = {}
        self.export_modules: Dict[str, Dict[str, Any]] = {}
    
    def export_function(self, name: str, func: Any, module_name: str = "lisp") -> None:
        """Export a Lisp function to Python"""
        if module_name not in self.export_modules:
            self.export_modules[module_name] = {}
        
        self.export_modules[module_name][name] = func
        self.exported_functions[name] = func
    
    def export_data(self, name: str, data: Any, module_name: str = "lisp") -> None:
        """Export Lisp data to Python"""
        if module_name not in self.export_modules:
            self.export_modules[module_name] = {}
        
        self.export_modules[module_name][name] = data
        self.exported_data[name] = data
    
    def create_python_module(self, module_name: str) -> Any:
        """Create a Python module from exported Lisp entities"""
        if module_name not in self.export_modules:
            return None
        
        # Create a module-like object
        module = type(sys.modules[__name__])(module_name)
        module.__dict__.update(self.export_modules[module_name])
        
        # Add to sys.modules so it can be imported
        sys.modules[module_name] = module
        
        return module
    
    def list_exports(self, module_name: str = "lisp") -> List[str]:
        """List exported entities for a module"""
        if module_name in self.export_modules:
            return list(self.export_modules[module_name].keys())
        return []

# Global Lisp exporter instance
lisp_exporter = LispExporter()

def export_to_python(name: str, entity: Any, module_name: str = "lisp") -> None:
    """Export a Lisp entity to Python"""
    if callable(entity):
        lisp_exporter.export_function(name, entity, module_name)
    else:
        lisp_exporter.export_data(name, entity, module_name)

def create_python_module(module_name: str) -> Any:
    """Create a Python module from exported Lisp entities"""
    return lisp_exporter.create_python_module(module_name)

def list_lisp_exports(module_name: str = "lisp") -> List[str]:
    """List exported Lisp entities"""
    return lisp_exporter.list_exports(module_name)

# Legacy compatibility functions (deprecated - use symbol_table functions instead)
def add_to_global_env(name: str, entity: Any) -> None:
    """Add an entity to the global environment (deprecated)"""
    add_symbol(name, entity)

def get_from_global_env(name: str) -> Optional[Any]:
    """Get an entity from the global environment (deprecated)"""
    return get_symbol(name)

def list_global_env() -> Dict[str, Any]:
    """List all entities in the global environment (deprecated)"""
    return list_symbols()

def get_imported(name: str) -> Optional[Any]:
    """Get an imported entity by name (deprecated)"""
    return get_symbol(name)
