#!/usr/bin/env python3

import subprocess
import os
from termcolor import colored

def run_command(command):
    """Run a shell command and capture its output and error message."""
    process = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
    stdout, stderr = process.communicate()
    return stdout.decode().strip(), stderr.decode().strip(), process.returncode

class TestFramework:
    def __init__(self):
        self.tests_run = 0
        self.tests_passed = 0

    def assert_equal(self, actual, expected, message):
        """Assert that actual is equal to expected."""
        self.tests_run += 1
        if actual == expected:
            self.tests_passed += 1
            print(colored(f"Test passed: {message}", "green"))
        else:
            print(colored(f"Test failed: {message} - Expected '{expected}', got '{actual}'", "red"))

    def report(self):
        """Report the test results."""
        print(f"\n{self.tests_passed}/{self.tests_run} tests passed")

def test_help(test_framework):
    """Test the help option."""
    output, error, _ = run_command("./glados -h")
    test_framework.assert_equal("Usage:" in output or "Usage:" in error, True, "Help test")

def read_file(file_path):
    """Read content from a file."""
    with open(file_path, 'r') as file:
        return file.read().strip()

def test_vm(file_path, expected_output, test_framework, file_name):
    """Test the VM with a given input file and expected output."""
    output, error, exit_code = run_command(f"./myvm -t {file_path}")
    actual_output = error if error else output
    test_framework.assert_equal(actual_output, expected_output, f"VM test for {file_name}")

def test_glados_text(source_folder, output_file, expected_output, test_framework):
    """Test Glados with specified source folder and output file."""
    run_command(f"./glados -t -s {source_folder} -o {output_file}")
    actual_output = read_file(output_file)
    test_framework.assert_equal(actual_output, expected_output, f"Glados test for source '{source_folder}'")

def test_glados_binary(source_folder, output_file, expected_output, test_framework):
    """Test Glados with specified source folder and output file."""
    run_command(f"./glados -b -s {source_folder} -o {output_file}")
    actual_output = read_file(output_file)
    test_framework.assert_equal(actual_output, expected_output, f"Glados test for source '{source_folder}'")

def test_glados_no_output(source_folder, expected_output, test_framework):
    """Test Glados with specified source folder and output file."""
    run_command(f"./glados -t -s {source_folder}")
    actual_output = read_file("a.out")
    test_framework.assert_equal(actual_output, expected_output, f"Glados test for source '{source_folder}'")

def test_glados_bad_args(source_folder, output_file, expected_output, test_framework):
    """Test Glados with specified source folder and output file."""
    actual_output, error, _ = run_command(f"./glados -t -s {source_folder} -o {output_file}")
    test_framework.assert_equal(actual_output, expected_output, f"Glados test for source '{source_folder}'")

def execute_command(file_name, result, test_framework):
    functionaltest_file = "functionaltest"
    command = f"./glados -t -s {file_name} -o {functionaltest_file}"

    try:
        with open(functionaltest_file, 'w') as f:
            pass
    except IOError as e:
        print(f"Error creating file {functionaltest_file}: {e}")
        return
    try:
        subprocess.run(command, shell=True, check=True)
        test_vm(functionaltest_file, result, test_framework, file_name)
    except subprocess.CalledProcessError as e:
        print(f"Error during command execution: {e}")
    finally:
        delete_file()

def delete_file():
    if os.path.exists("functionaltest"):
        try:
            os.remove("functionaltest")
        except OSError as e:
            print(f"Error during file deletion: {e}")
    else:
        print(f"The file functionaltest does not exist.")

def main():
    test_framework = TestFramework()

    # Add your tests here
    execute_command("test/example/addition.txt", "6", test_framework)
    execute_command("test/example/subtraction.txt", "4", test_framework)
    execute_command("test/example/multiplication.txt", "10", test_framework)
    execute_command("test/example/divided.txt", "5", test_framework)
    execute_command("test/example/variable_easy.txt", "5", test_framework)
    execute_command("test/example/variable_medium.txt", "16", test_framework)
    execute_command("test/example/variable_hard.txt", "-6", test_framework)
    execute_command("test/example/if_easy.txt", "yes", test_framework)
    execute_command("test/example/if_medium.txt", "yes", test_framework)
    execute_command("test/example/else.txt", "yes", test_framework)
    execute_command("test/example/or_medium.txt", "yes", test_framework)
    execute_command("test/example/and_medium.txt", "yes", test_framework)
    execute_command("test/example/while_easy.txt", "5", test_framework)
    execute_command("test/example/while_medium.txt", "5", test_framework)
    execute_command("test/example/while_hard.txt", "100", test_framework)
    execute_command("test/example/function_easy.txt", "yes", test_framework)
    execute_command("test/example/function_medium.txt", "yes", test_framework)
    execute_command("test/example/function_hard.txt", "yes", test_framework)
    execute_command("test/example/return_easy.txt", "5", test_framework)
    execute_command("test/example/return_medium.txt", "1", test_framework)
    execute_command("test/example/return_hard.txt", "1", test_framework)
    execute_command("test/example/factorial.txt", "120", test_framework)
    test_cases_text = [
        ("test/example/glados-test/example1", "test/example/glados-test/output1.txt", """IPushString "helloWorld"
IPrint"""),
        ("test/example/glados-test/example1", "test/example/glados-test/test", """IPushString "helloWorld"
IPrint"""),
    ]

    tests_cases_binary = [
        ("test/example/glados-test/exampleBinary", "test/example/glados-test/outputBinary.txt", """10610011000000000000000a40081062001100000000000000144008"""),
    ]
    test_cases_no_output = [
        ("test/example/glados-test/example2", """IPushString "helloWorld"
IPrint"""),
    ]
    test_cases_bad_args = [
        ("test/example/glados-test/example3", "test/example/glados-test/output3.txt", """Error reading file 'test/example/glados-test/example3': test/example/glados-test/example3: openFile: does not exist (No such file or directory)"""),
    ]

    for source_folder, output_file, expected_output in test_cases_text:
        test_glados_text(source_folder, output_file, expected_output, test_framework)
    for source_folder, output_file, expected_output in tests_cases_binary:
        test_glados_binary(source_folder, output_file, expected_output, test_framework)
    for source_folder, expected_output in test_cases_no_output:
        test_glados_no_output(source_folder, expected_output, test_framework)
    for source_folder, output_file, expected_output in test_cases_bad_args:
        test_glados_bad_args(source_folder, output_file, expected_output, test_framework)

    test_help(test_framework)
    # Report the test results
    test_framework.report()

if __name__ == "__main__":
    main()