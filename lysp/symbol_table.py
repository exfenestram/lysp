from __future__ import annotations
from typing import Any, Dict, Optional, Set
import threading

class SymbolTable:
    """Centralized symbol table for managing all symbols in the Lisp environment"""
    
    def __init__(self):
        self._symbols: Dict[str, Any] = {}
        self._lock = threading.Lock()
    
    def add_symbol(self, name: str, value: Any) -> None:
        """Add a symbol to the symbol table"""
        with self._lock:
            self._symbols[name] = value
    
    def get_symbol(self, name: str) -> Optional[Any]:
        """Get a symbol from the symbol table"""
        with self._lock:
            return self._symbols.get(name)
    
    def has_symbol(self, name: str) -> bool:
        """Check if a symbol exists in the symbol table"""
        with self._lock:
            return name in self._symbols
    
    def remove_symbol(self, name: str) -> None:
        """Remove a symbol from the symbol table"""
        with self._lock:
            if name in self._symbols:
                del self._symbols[name]
    
    def list_symbols(self) -> Dict[str, Any]:
        """List all symbols in the symbol table"""
        with self._lock:
            return self._symbols.copy()
    
    def clear(self) -> None:
        """Clear all symbols from the symbol table"""
        with self._lock:
            self._symbols.clear()
    
    def update_from_dict(self, symbols: Dict[str, Any]) -> None:
        """Update the symbol table from a dictionary"""
        with self._lock:
            self._symbols.update(symbols)
    
    def get_all_symbols(self) -> Dict[str, Any]:
        """Get all symbols as a dictionary"""
        with self._lock:
            return self._symbols.copy()

# Global symbol table instance
_symbol_table = SymbolTable()

def add_symbol(name: str, value: Any) -> None:
    """Add a symbol to the global symbol table"""
    _symbol_table.add_symbol(name, value)

def get_symbol(name: str) -> Optional[Any]:
    """Get a symbol from the global symbol table"""
    return _symbol_table.get_symbol(name)

def has_symbol(name: str) -> bool:
    """Check if a symbol exists in the global symbol table"""
    return _symbol_table.has_symbol(name)

def remove_symbol(name: str) -> None:
    """Remove a symbol from the global symbol table"""
    _symbol_table.remove_symbol(name)

def list_symbols() -> Dict[str, Any]:
    """List all symbols in the global symbol table"""
    return _symbol_table.list_symbols()

def clear_symbols() -> None:
    """Clear all symbols from the global symbol table"""
    _symbol_table.clear()

def update_symbols(symbols: Dict[str, Any]) -> None:
    """Update the global symbol table from a dictionary"""
    _symbol_table.update_from_dict(symbols)

def get_all_symbols() -> Dict[str, Any]:
    """Get all symbols as a dictionary"""
    return _symbol_table.get_all_symbols()
