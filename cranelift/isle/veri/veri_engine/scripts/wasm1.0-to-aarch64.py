import subprocess
import os
from datetime import datetime
from collections import defaultdict
import tabulate

"""The prefix we use for tests based on named rules within aarch64 files."""
TEST_PREFIX = "test_named"

"""The suffix we use for tests that use Crocus as an interpreter.
This are for validating our annotations and should not count
toward the total."""
TEST_SKIP_COUNT = "_concrete"

SUCCESS = "Verification succeeded"
NOT_APPLICABLE = "Rule not applicable"
TIMEOUT = "Environment variable SKIP_SOLVER set, returning Unknown"
FAILED = "Verification failed"

EXPECTED_FAILURES = 2
EXPECTED_FAILURES_INVOCATIONS = 4

"""Run Crocus on the various rules by invoking subsets of Rust tests."""
def verify_rules(tests, timeout_tests):
    runtimes = []

    now = datetime.now().strftime("%Y-%m-%dT%H:%M:%S%z")
    filename = "script-results/wasm-1.0-to-aarch64-log-%s.txt" % now
    print("Writing log to %s" % filename)

    with open(filename, "w") as file:
    
        print("Verifying Wasm 1.0 to Aarch64 tests, expecting %d tests to succeed" % len(tests))
        cmd = [
            "cargo", 
            "test", 
            TEST_PREFIX, 
            "--", 
            "--skip",
            TEST_SKIP_COUNT,
            "--test-threads=1",
            "--nocapture"
        ]
        print("Expected to take approximately 5 minutes")
        print(" ".join(cmd))
        subprocess.call(cmd, stdout=file)
    
        print("Counting Wasm 1.0 to Aarch64 tests marked as timing out (skipping solver), expecting %d" % len(tests))
        env = os.environ
        env["SKIP_SOLVER"] = "1"
        cmd = [
            "cargo", 
            "test", 
            TEST_PREFIX, 
            "--", 
            "--ignored",
            "--test-threads=1",
            "--nocapture"
        ]
        print("Expected to take approximately 1 minute")
        print(" ".join(cmd))
        subprocess.call(cmd, stdout=file, env=env)

    count_rule_results(filename, tests, timeout_tests)


def count_rule_results(filename, tests, timeout_tests):

    test_results = defaultdict(list)
    current_test = None

    on = False
    with open(filename, "r") as file:
        # Process the log line by line
        while True:
            line = file.readline()
        
            # if line is empty
            # end of file is reached
            if not line:
                break

            if "VERIFYING rule with name:" in line:
                # Every test but the first must have at least one result
                assert(not (current_test and len(test_results[current_test]) == 0))
                current_test = line.replace("VERIFYING rule with name:", "").strip()
                if on:
                    print("Result not found for: ", current_test)
                    exit(1)
                on = True
            elif SUCCESS in line:
                assert(current_test != None)
                test_results[current_test].append(SUCCESS)
                on = False
            elif NOT_APPLICABLE in line:
                assert(current_test != None)
                test_results[current_test].append(NOT_APPLICABLE)
                on = False
            elif FAILED in line:
                assert(current_test != None)
                test_results[current_test].append(FAILED)
                on = False
            elif TIMEOUT in line:
                assert(current_test != None)
                test_results[current_test].append(TIMEOUT)
                on = False

        total_success = 0
        total_inapplicable = 0
        total_timeout = 0
        total_failed = 0

        rules_has_some_success = 0
        rules_has_only_success_or_inapplicable = 0
        rules_has_some_timeout = 0
        rules_has_only_timeout = 0
        rules_has_some_failure = 0
        for test, results in test_results.items():
            rule_has_some_success = False
            rule_has_some_timeout = False
            rule_has_some_failure = False
            rule_has_only_success_or_inapplicable = True
            rule_has_only_timeout = True
            for result in results:
                if result == SUCCESS:
                    total_success += 1
                    rule_has_some_success = True
                    rule_has_only_timeout = False
                if result == NOT_APPLICABLE:
                    total_inapplicable += 1
                if result == FAILED:
                    total_failed += 1
                    rule_has_some_failure = True
                    rule_has_only_success_or_inapplicable = False
                if result == TIMEOUT:
                    total_timeout += 1
                    rule_has_some_timeout = True
                    rule_has_only_success_or_inapplicable = False
            # After looping per rule
            if rule_has_some_success:
                rules_has_some_success += 1
            if rule_has_some_timeout:
                rules_has_some_timeout += 1
            if rule_has_only_success_or_inapplicable:
                rules_has_only_success_or_inapplicable += 1 
            if rule_has_only_timeout:
                rules_has_only_timeout += 1
            if rule_has_some_failure:
                rules_has_some_failure += 1

        # Check for the expected number of false failures
        assert(rules_has_some_failure == EXPECTED_FAILURES)
        assert(total_failed == EXPECTED_FAILURES_INVOCATIONS)

        # Check the expected of timeout tests
        assert(rules_has_some_timeout == len(timeout_tests))

        # Build ASCII table for artifact evaluation
        results = []
        results.append(["", "Total", "Success", "Timeout", "Inapplicable", "Failure"])
        results.append([
            "Rules", 
            # Total
            len(test_results.keys()), 
            # Success
            "%d (all types) / %d (any type)" % (rules_has_only_success_or_inapplicable, rules_has_some_success), 
            # Timeout
            "%d (any type) / %d (all types)" % (rules_has_some_timeout, rules_has_only_timeout), 
            # Inapplicable
            "N/A",
            # Failure
            "%d (0)" % (rules_has_some_failure), 
        ])
        results.append([
            "Type Insts.", 
            # Total
            total_timeout + total_inapplicable + total_success, 
            # Success
            "%d" % total_success, 
            # Timeout
            "%d" % total_timeout, 
            # Inapplicable
            "%d" % total_inapplicable, 
            # Failure
            "%d (0)" %  total_failed, 
        ])   

        # Print LaTeX to include
        print("LaTeX-formatted:")
        latex = [
            "\ifstrequal{#1}{rules}{%s}{}" % results[1][1],
            "\ifstrequal{#1}{rules-succeeded}{%s}{}" % results[1][2],
            "\ifstrequal{#1}{rules-timedout}{%s}{}" % results[1][3],
            "\ifstrequal{#1}{rules-inapplicable}{%s}{}" % results[1][4],
            "\ifstrequal{#1}{rules-failed}{%s}{}" % results[1][5],
            "\ifstrequal{#1}{type-invs}{%s}{}" % results[2][1],
            "\ifstrequal{#1}{invs-succeeded}{%s}{}" % results[2][2],
            "\ifstrequal{#1}{invs-timedout}{%s}{}" % results[2][3],
            "\ifstrequal{#1}{invs-inapplicable}{%s}{}" % results[2][4],
            "\ifstrequal{#1}{invs-failed}{%s}{}" % results[2][5],
            "\ifstrequal{#1}{type-invs-term}{%d}{}" % (total_inapplicable + total_success),
            "\ifstrequal{#1}{rules-timedout-number}{%d}{}" % rules_has_some_timeout,
        ]
        print("%\n".join(latex) + "%")

        print("\nASCII-formatted:")
        print(tabulate.tabulate(results,headers="firstrow"))

            



"""Script to run our test suite for Wasm 1.0 to Aarch64 rules."""
if __name__ == "__main__":

    with open("wasm-1.0-to-aarch64-tests/expected_success_tests.txt") as file:
        expected_success_tests = [line.rstrip() for line in file]

    with open("wasm-1.0-to-aarch64-tests/expected_timeout_tests.txt") as file:
        timeout_tests = [line.rstrip() for line in file]
    
    verify_rules(expected_success_tests, timeout_tests)
