from __future__ import annotations
# Install the .lysp/.lisp module import hook automatically
try:
	from .loader import install as _install_lysp_loader
	_install_lysp_loader()
except Exception:
	# Non-fatal if loader can't be installed
	pass
