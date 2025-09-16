#!/usr/bin/env python3
"""
Gauntlet Script for P4 Program Generation and Testing

This script generates P4 programs using p4smith, tests them with p4spectec,
and categorizes them based on their type checking results.

Usage:
    python3 gauntlet.py --duration 3600  # Run for 1 hour
    python3 gauntlet.py --count 100      # Generate 100 programs
"""

import os
import sys
import subprocess
import shutil
import argparse
import time
import glob
from pathlib import Path
from typing import Tuple, Optional
from coverage_utils import read_coverage, write_coverage, union_coverage

class GauntletRunner:
    def __init__(self, project_root: str, loop_size: int):
        self.project_root = Path(project_root)
        self.p4c_build_dir = self.project_root / "p4c" / "build"
        self.gauntlet_dir = self.project_root / "gauntlet"
        self.spec_dir = self.project_root / "spec"
        self.p4include_dir = self.project_root / "p4c" / "p4include"
        self.p4spectec_binary = self.project_root / "p4spectec"
        self.loop_size = max(1, int(loop_size))
        
        # Backup existing gauntlet directory if it exists
        self._backup_existing_gauntlet()
        
        # Create fresh gauntlet directory and subdirectories
        self.gauntlet_dir.mkdir(exist_ok=True)
    
    def _backup_existing_gauntlet(self):
        """Backup existing gauntlet directory if it exists."""
        if self.gauntlet_dir.exists():
            # Generate timestamp for backup name
            timestamp = time.strftime("%Y%m%d_%H%M%S")
            backup_name = f"gauntlet_backup_{timestamp}"
            backup_path = self.project_root / backup_name
            
            print(f"Backing up existing gauntlet directory to {backup_name}...")
            try:
                shutil.move(str(self.gauntlet_dir), str(backup_path))
                print(f"Successfully backed up to {backup_name}")
            except Exception as e:
                print(f"ERROR: Failed to backup gauntlet directory: {e}")
                sys.exit(1)
    
    def check_dependencies(self) -> bool:
        """Check if all required tools and directories exist."""
        print("Checking dependencies...")
        
        # Check if p4spectec exists
        if not self.p4spectec_binary.exists():
            print(f"ERROR: p4spectec not found at {self.p4spectec_binary}")
            return False
        
        # Check if p4smith exists (we'll try to find it)
        p4smith_path = self.find_p4smith()
        if not p4smith_path:
            print("ERROR: p4smith not found. You may need to build it first.")
            print("To build p4smith:")
            print("  cd p4c")
            print("  mkdir build && cd build")
            print("  cmake ..")
            print("  make")
            return False
        
        # Check if spec directory exists
        if not self.spec_dir.exists():
            print(f"ERROR: spec directory not found at {self.spec_dir}")
            return False
        
        # Check if p4include directory exists
        if not self.p4include_dir.exists():
            print(f"ERROR: p4include directory not found at {self.p4include_dir}")
            return False
        
        print("All dependencies found!")
        return True
    
    def find_p4smith(self) -> Optional[Path]:
        """Try to find p4smith binary in various locations."""
        possible_paths = [
            Path("/usr/local/bin/p4smith"),
            Path("/opt/homebrew/bin/p4smith"),
            self.p4c_build_dir / "p4smith",
        ]
        
        for path in possible_paths:
            if path.exists() and path.is_file():
                return path
        
        # Try to find it in PATH
        try:
            result = subprocess.run(["which", "p4smith"], 
                                  capture_output=True, text=True, check=True)
            return Path(result.stdout.strip())
        except subprocess.CalledProcessError:
            pass
        
        return None
    
    def generate_p4_program(self, program_name: str) -> bool:
        """Generate a P4 program using p4smith."""
        p4smith_path = self.find_p4smith()
        if not p4smith_path:
            print("ERROR: p4smith not available")
            return False
        
        output_file = self.p4c_build_dir / f"{program_name}.p4"
        
        try:
            # Change to p4c/build directory and run p4smith
            cmd = [
                str(p4smith_path),
                "--target", "bmv2",
                "--arch", "v1model",
                f"{program_name}.p4"
            ]
            
            print(f"Generating {program_name}.p4...")
            result = subprocess.run(
                cmd,
                cwd=self.p4c_build_dir,
                capture_output=True,
                text=True,
                timeout=30  # 30 second timeout
            )

            if result.returncode != 0:
                print(f"ERROR: p4smith failed: {result.stderr}")
                return False

            if not output_file.exists():
                print(f"ERROR: Generated file {output_file} not found")
                return False

            print(f"Successfully generated {program_name}.p4")
            return True

        except subprocess.TimeoutExpired:
            print(f"ERROR: p4smith timed out for {program_name}")
            return False
        except Exception as e:
            print(f"ERROR: Failed to run p4smith: {e}")
            return False

    def test_p4_program(self, program_name: str) -> Tuple[str, str]:
        """
        Test a P4 program using p4spectec.
        Returns (result_type, output) where result_type is one of:
        'well-typed', 'ill-typed', 'ill-formed', or 'error'
        """
        p4_file = self.p4c_build_dir / f"{program_name}.p4"

        if not p4_file.exists():
            return "error", f"Program file {p4_file} not found"
        
        try:
            # Collect .watsup files in a deterministic order (alphabetical)
            watsup_files = sorted(
                self.spec_dir.glob("*.watsup"), key=lambda p: p.name
            )
            if not watsup_files:
                return "error", "No .watsup files found in spec"

            # Build the p4spectec command with explicit, ordered arguments
            cmd = [
                str(self.p4spectec_binary),
                "run-sl",
            ] + [str(f) for f in watsup_files] + [
                "-i", str(self.p4include_dir),
                "-p", str(p4_file)
            ]
            
            print(f"Testing {program_name}.p4...")
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=60  # 60 second timeout
            )
            
            output = result.stdout + result.stderr
            
            # Parse the output to determine the result type
            if "well-typed" in output.lower():
                return "well-typed", output
            elif "ill-typed" in output.lower():
                return "ill-typed", output
            elif "ill-formed" in output.lower():
                return "ill-formed", output
            else:
                return "error", output
                
        except subprocess.TimeoutExpired:
            return "timeout", "p4spectec timed out"
        except Exception as e:
            return "error", f"Failed to run p4spectec: {e}"
    
    def _loop_dir_for_program(self, program_name: str) -> Path:
        """Compute loop directory path based on program index and loop size."""
        # Expect names like gauntlet00001; extract the numeric suffix
        num_part = ''.join(ch for ch in program_name if ch.isdigit())
        try:
            idx = int(num_part)
        except ValueError:
            idx = 0
        loop_index = idx // self.loop_size
        loop_dir = self.gauntlet_dir / f"loop{loop_index:02d}"
        return loop_dir

    def categorize_program(self, program_name: str, result_type: str) -> bool:
        """Move the program to the appropriate directory based on result type."""
        source_file = self.p4c_build_dir / f"{program_name}.p4"
        
        if not source_file.exists():
            print(f"ERROR: Source file {source_file} not found")
            return False
        
        # Determine destination directory under the appropriate loop folder
        loop_dir = self._loop_dir_for_program(program_name)
        dest_dir: Path
        if result_type == "well-typed":
            dest_dir = loop_dir / "welltyped"
        elif result_type == "ill-typed":
            dest_dir = loop_dir / "illtyped"
        elif result_type == "ill-formed":
            dest_dir = loop_dir / "illformed"
        elif result_type == "timeout":
            dest_dir = loop_dir / "timeout"
        else:
            print(f"ERROR: Unknown result type: {result_type}")
            return False
        
        # Ensure destination directories exist
        dest_dir.mkdir(parents=True, exist_ok=True)
        dest_file = dest_dir / f"{program_name}.p4"
        
        try:
            shutil.move(str(source_file), str(dest_file))
            print(f"Moved {program_name}.p4 to {dest_dir.name}/")
            return True
        except Exception as e:
            print(f"ERROR: Failed to move {program_name}.p4: {e}")
            return False
    
    def run_gauntlet(self, duration: Optional[int] = None, count: Optional[int] = None):
        """Run the gauntlet for specified duration or count."""
        if not self.check_dependencies():
            print("Dependency check failed. Exiting.")
            return
        
        start_time = time.time()
        program_count = 0
        
        print(f"Starting gauntlet run...")
        if duration:
            print(f"Duration: {duration} seconds")
        if count:
            print(f"Count: {count} programs")
        
        try:
            last_measured_loop_index = -1
            while True:
                # Check if we should stop
                if duration and (time.time() - start_time) >= duration:
                    print(f"Duration limit reached ({duration}s)")
                    break
                
                if count and program_count >= count:
                    print(f"Count limit reached ({count} programs)")
                    break
                
                # Generate program name
                program_name = f"gauntlet{program_count:05d}"
                program_count += 1
                
                # Generate P4 program
                if not self.generate_p4_program(program_name):
                    print(f"Failed to generate {program_name}, skipping...")
                    continue
                
                # Test the program
                result_type, output = self.test_p4_program(program_name)
                
                if result_type == "error":
                    print(f"Error testing {program_name}: {output}")
                    # Clean up the generated file
                    temp_file = self.p4c_build_dir / f"{program_name}.p4"
                    if temp_file.exists():
                        temp_file.unlink()
                    continue
                
                # Categorize and move the program
                if not self.categorize_program(program_name, result_type):
                    print(f"Failed to categorize {program_name}")
                    continue
                
                print(f"Processed {program_name}: {result_type}")
                
                # Print summary every 10 programs
                if program_count % 10 == 0:
                    self.print_summary()

                # If we completed a loop, measure coverage for that loop
                if program_count % self.loop_size == 0:
                    loop_index = (program_count - 1) // self.loop_size
                    self.measure_loop_coverage(loop_index)
                    last_measured_loop_index = loop_index
        
        except KeyboardInterrupt:
            print("\nInterrupted by user")
        
        print(f"\nGauntlet completed. Processed {program_count} programs.")
        # Measure coverage for the final (possibly partial) loop if not measured
        if program_count > 0:
            final_loop_index = (program_count - 1) // self.loop_size
            if final_loop_index != last_measured_loop_index:
                self.measure_loop_coverage(final_loop_index)
        self.print_summary()
    
    def print_summary(self):
        """Print a summary of categorized programs across all loop directories."""
        welltyped_count = 0
        illtyped_count = 0
        illformed_count = 0
        timeout_count = 0

        for loop_dir in sorted(self.gauntlet_dir.glob("loop*")):
            welltyped_count += len(list((loop_dir / "welltyped").glob("*.p4")))
            illtyped_count += len(list((loop_dir / "illtyped").glob("*.p4")))
            illformed_count += len(list((loop_dir / "illformed").glob("*.p4")))
            timeout_count += len(list((loop_dir / "timeout").glob("*.p4")))
        
        print(f"\nSummary:")
        print(f"  Well-typed: {welltyped_count}")
        print(f"  Ill-typed:  {illtyped_count}")
        print(f"  Ill-formed: {illformed_count}")
        print(f"  Timeout:    {timeout_count}")
        print(f"  Total:      {welltyped_count + illtyped_count + illformed_count + timeout_count}")

    def measure_loop_coverage(self, loop_index: int) -> None:
        """Measure coverage for a given loop directory and union with previous loop."""
        loop_name = f"loop{loop_index:02d}"
        loop_dir = self.gauntlet_dir / loop_name
        cov_file = self.gauntlet_dir / f"{loop_name}.cov"

        # Collect .watsup files in deterministic order
        watsup_files = sorted(self.spec_dir.glob("*.watsup"), key=lambda p: p.name)
        if not watsup_files:
            print("No .watsup files found for coverage; skipping.")
            return

        cmd = [
            str(self.p4spectec_binary),
            "cover-dangling",
        ] + [str(f) for f in watsup_files] + [
            "-i", str(self.project_root / "p4c" / "p4include"),
            "-d", str(loop_dir),
            "-cov", str(cov_file),
            "-ignore", str(self.project_root / "ignores" / "function.ignore"),
            "-ignore", str(self.project_root / "ignores" / "relation.ignore"),
        ]

        print(f"Measuring coverage for {loop_name}...")
        try:
            result = subprocess.run(
                cmd,
                cwd=self.project_root,
                capture_output=True,
                text=True,
                timeout=600,
            )
            if result.returncode != 0:
                print(f"Coverage measurement failed for {loop_name}: {result.stderr}")
                return
        except subprocess.TimeoutExpired:
            print(f"Coverage measurement timed out for {loop_name}")
            return
        except Exception as e:
            print(f"Coverage measurement error for {loop_name}: {e}")
            return

        # Union with previous loop's coverage if present
        if loop_index > 0:
            prev_name = f"loop{loop_index - 1:02d}"
            prev_cov = self.project_root / f"{prev_name}.cov"
            if prev_cov.exists():
                try:
                    prev = read_coverage(str(prev_cov))
                    curr = read_coverage(str(cov_file))
                    joined = union_coverage(prev, curr)
                    write_coverage(str(cov_file), joined)
                    print(f"Updated {cov_file.name} with union of {prev_name}.cov")
                except Exception as e:
                    print(f"Failed to union coverage for {loop_name}: {e}")


def main():
    parser = argparse.ArgumentParser(description="P4 Program Gauntlet Runner")
    parser.add_argument("--duration", type=int, help="Run for specified duration in seconds")
    parser.add_argument("--count", type=int, help="Generate specified number of programs")
    parser.add_argument("--project-root", default=".", help="Project root directory")
    parser.add_argument("--loop-size", type=int, default=1000, help="Programs per loop directory (default: 1000)")
    
    args = parser.parse_args()
    
    if not args.duration and not args.count:
        parser.error("Must specify either --duration or --count")
    
    if args.duration and args.count:
        parser.error("Cannot specify both --duration and --count")
    
    # Get absolute path to project root
    project_root = os.path.abspath(args.project_root)
    
    runner = GauntletRunner(project_root, loop_size=args.loop_size)
    runner.run_gauntlet(duration=args.duration, count=args.count)


if __name__ == "__main__":
    main()

