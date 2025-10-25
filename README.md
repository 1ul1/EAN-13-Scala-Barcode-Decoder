# Barcode Decoder

A Scala implementation of an EAN-13 barcode decoding system that processes binary image data to extract and validate product information from barcode images.

## Project Overview

The project implements core functional programming concepts through the development of a complete barcode decoding pipeline. The system transforms raw pixel data into valid EAN-13 codes using pattern recognition algorithms and mathematical validation.

## Technical Implementation

### Core Functional Programming Components

**Basic Operations & Data Structures**
- Bit type conversions and complement operations
- Run-length encoding for sequence compression
- List grouping algorithms for pattern detection
- Rational number arithmetic for precise calculations

**Pattern Recognition System**
- Three encoding schemes (L, G, R) for EAN-13 digit representation
- Relative frequency analysis for robust pattern matching
- Parity-based digit classification
- Minimum distance algorithms for optimal pattern matching

**Validation Framework**
- Checksum digit calculation and verification
- Parity pattern validation
- Complete EAN-13 specification compliance checking

## Key Algorithms Implemented

### 1. Data Processing Pipeline
- `runLength()`: Converts binary sequences to compressed run-length format
- `scaleToOne()`: Normalizes measurements to relative frequencies
- `group()`: Identifies consecutive identical elements

### 2. Pattern Matching Engine
- `bestMatch()`: Finds closest digit pattern using distance metrics
- `distance()`: Calculates similarity between encoding patterns
- `bestLeft()` / `bestRight()`: Specialized matching for left/right digit groups

### 3. Validation System
- `firstDigit()`: Derives leading digit from parity patterns
- `checkDigit()`: Computes and validates checksum
- `verifyCode()`: Comprehensive barcode validation
- `solve()`: Complete decoding pipeline

## EAN-13 Decoding Specification

The implementation handles the complete EAN-13 standard:
- **13-digit structure** with parity encoding
- **Triple encoding schemes** (L-odd, G-even, R-right patterns)
- **Parity-based first digit determination**
- **Modulo-10 checksum validation**

## Features
1)
This project demonstrates advanced functional programming concepts:
- Higher-order functions and immutable data structures
- Pattern matching algorithms
- Mathematical modeling of real-world specifications
- Error detection and validation systems
- Type-safe Scala programming practices
2)
- **Core Decoding Logic**: Complete implementation of EAN-13 specification
- **Mathematical Foundation**: Rational number arithmetic and distance calculations
- **Pattern Recognition**: Advanced matching algorithms for robust decoding
- **Validation Pipeline**: Multi-stage verification for accurate results

## Technical Achievement

Successfully implements a production-grade barcode decoding system using pure functional programming principles, providing reliable EAN-13 code extraction from binary image data with built-in error detection and validation.