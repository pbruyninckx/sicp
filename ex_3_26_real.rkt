#lang sicp

; Use a nested binary tree as representation.
; The challenge will be to ensure that the difference
; between the different trees is clear.

; Let's try to sketch this

; ('*table _tree_)
; with _tree_:
; (key value (or _tree_ '()) (or _tree_ '())
; In order to allow for multi-dimensional trees,
; value can then be either a plain value,
; or another tree.

; Note that all values in each left subtree should be smaller
; then the main value, which should be smaller than all values
; in each right subtree.

; Note that this tree won't have any self-balancing properties.
