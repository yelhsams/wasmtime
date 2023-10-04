import time
import matplotlib.pyplot as plt
import numpy as np
import subprocess
from datetime import datetime

"""Use `time` to record each test individually. Note this counts Rust test overhead."""
def measure_verification_runtime(tests):
    runtimes = []
    
    for test in tests:
        print("TIMING: " + test)
        start_time = time.time()
        result = subprocess.check_output(["cargo", "test", test, "--", "--exact"])
        end_time = time.time()

        if "1 passed" in str(result): 
            runtime = end_time - start_time
            runtimes.append(runtime)
        else:
            print("Test unexpectedly failed!")
            print(result)
            exit(1)
    
    return runtimes

"""Create the CDF with `matplotlib`"""
def plot_verification_runtime_cdf(runtimes, n):

    # Set tests that timeout to have large runtimes for the purposes of this 
    # figure, the exact number doesn't matter because we set the axes explicitly 
    runtimes += [10000000 for i in range(n - len(runtimes))]
    runtimes.sort()
    y = np.arange(1, n+1) / n

    plt.figure(figsize=(5, 2))
    plt.subplots_adjust(bottom=0.18)
    plt.step(runtimes, y, where="post")
    plt.xlabel('Verification times (seconds)', fontsize=9)
    plt.ylabel('CDF', fontsize=9)
    plt.title('CDF of Verification Times', fontsize=9)
    plt.tick_params(axis='both', which='major', labelsize=6)
    plt.grid()
    plt.xlim(0, 25)
    plt.ylim(0, 1)

    # Timestamp
    now = datetime.now().strftime("%Y-%m-%dT%H:%M:%S%z")

    print("Generating results with timestamp", now)

    # Save the resulting PDF
    pdf = "script-results/cdf-%s.pdf" % now
    print("Saving PDF to", pdf)
    plt.savefig(pdf)

    # Save these results to a file 
    filename = "script-results/cdf-results-%s.txt" % now
    print("Saving raw results to", filename)
    with open(filename, 'w') as file:
        file.writelines('\n'.join(["{:.6f}".format(t) for t in runtimes]))

"""Script to generate a CDF from verification times for specific lists of expected
successful and timeout tests"""
if __name__ == "__main__":

    # Fix ACM template font issue
    plt.rcParams['pdf.fonttype'] = 42
    plt.rcParams['ps.fonttype'] = 42

    with open("wasm-1.0-to-aarch64-tests/expected_success_tests.txt") as file:
        named_tests = [line.rstrip() for line in file]

    with open("wasm-1.0-to-aarch64-tests/expected_timeout_tests.txt") as file:
        timeout_tests = [line.rstrip() for line in file]
    
    success_runtimes = measure_verification_runtime(named_tests)
    assert(len(named_tests) == len(success_runtimes))

    # Total for the CDF is the number of successful tests plus the length of the 
    # expected timeout tests
    n = len(success_runtimes) + len(timeout_tests)

    # Plot the CDF
    plot_verification_runtime_cdf(success_runtimes, n)