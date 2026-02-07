# file2.py - Sample Python file

def process_data(data):
    # TODO: Add input validation
    # BUG: Crashes on empty list
    return sorted(data)

def export_results(results):
    # OPTIMIZE: Use bulk insert instead of loop
    for r in results:
        save(r)
