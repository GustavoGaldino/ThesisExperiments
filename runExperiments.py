from os import system, makedirs
from sys import argv
from enum import Enum
from typing import Optional

class DataStructure(Enum):
    SimpleQueue = 'SimpleQueue'
supported_data_structures = [e.value for e in DataStructure]

class Experiment(Enum):
    Add = 'Add'
    AddAll = 'AddAll'
    Clear = 'Clear'
    Contains = 'Contains'
    ContainsAll = 'ContainsAll'
    Iterator = 'Iterator'
    Remove = 'Remove'
    RemoveAll = 'RemoveAll'
    RetainAll = 'RetainAll'
    ToList = 'ToList'
supported_experiments = [e.value for e in Experiment]

def get_parameters_default_for_experiment(experiment: Experiment) -> tuple[int, int, Optional[int]]:
    match experiment:
        case Experiment.Add:
            return (1, 100000, 100000)
        case Experiment.AddAll:
            return (1000, 100000, 1000)
        case Experiment.Clear:
            return (1, 100000, None)
        case Experiment.Contains:
            return (1000, 100000, None)
        case Experiment.ContainsAll:
            return (5000, 100000, 1000)
        case Experiment.Iterator:
            return (1, 100000, None)
        case Experiment.Remove:
            return (10000, 100000, None)
        case Experiment.RemoveAll:
            return (10, 100000, 1000)
        case Experiment.RetainAll:
            return (10, 100000, 1000)
        case Experiment.ToList:
            return (5000, 100000, None)

# List how parameters will change to collect data about an experiment
# Run the experiment with all values of one of the paremeters while others are fixed by get_parameters_default_for_experiment
def get_parameters_for_experiment(experiment: Experiment) -> tuple[list[int], list[int], list[Optional[int]]]:
    match experiment:
        case Experiment.Add:
            return ([1], [1, 10, 100, 1000, 10000, 100000], [1, 10, 100, 1000, 10000, 100000])
        case Experiment.AddAll:
            return ([1, 10, 100, 1000], [1, 10, 100, 1000, 10000, 100000], [1, 10, 100, 1000])
        case Experiment.Clear:
            return ([1], [1, 10, 100, 1000, 10000, 100000], [None])
        case Experiment.Contains:
            return ([1, 10, 100, 1000], [1, 10, 100, 1000, 10000, 100000], [None])
        case Experiment.ContainsAll:
            return ([5, 50, 500, 5000], [1, 10, 100, 1000, 10000, 100000], [1, 10, 100, 1000])
        case Experiment.Iterator:
            return ([1], [1, 10, 100, 1000, 10000, 100000], [None])
        case Experiment.Remove:
            return ([1, 10, 100, 1000, 10000], [1, 10, 100, 1000, 10000, 100000], [None])
        case Experiment.RemoveAll:
            return ([1, 10], [1, 10, 100, 1000, 10000, 100000], [1, 10, 100, 1000])
        case Experiment.RetainAll:
            return ([1, 10], [1, 10, 100, 1000, 10000, 100000], [1, 10, 100, 1000])
        case Experiment.ToList:
            return ([5, 50, 500, 5000], [1, 10, 100, 1000, 10000, 100000], [None])

# Reads tg.prof (profiler output) file and extract time, allocation, processor and dram energy data
def get_experiment_result_data() -> tuple[float, int, float, float]:
    with open('tg.prof', 'r', encoding='ASCII') as prof_file:
        lines = prof_file.readlines()
        time = float([line for line in lines if 'total time' in line][0].split('=')[1].split('secs')[0])
        alloc = int([line for line in lines if 'total alloc' in line][0].split('=')[1].split('bytes')[0].replace(',', ''))
        energy_pkg = float([line for line in lines if 'total energy pkg' in line][0].split('=')[1].split('joules')[0])
        energy_dram = float([line for line in lines if 'total energy dram' in line][0].split('=')[1].split('joules')[0])
        return (time, alloc, energy_pkg, energy_dram)

# Returns validated data structure and experiment
def parse_args(args: list[str]) -> tuple[DataStructure, Experiment]:
    # First argument is the script name
    if len(args) < 3:
        raise ValueError('Not enough arguments. Specify a data structure and an experiment.')

    if args[1] not in supported_data_structures:
        raise ValueError('Invalid data structure. Pick one of: {0}'.format(supported_data_structures))

    if args[2] not in supported_experiments:
        raise ValueError('Invalid experiment. Pick one of: {0}'.format(supported_experiments))

    return DataStructure[args[1]], Experiment[args[2]]

def run_experiment(data_structure: DataStructure, experiment: Experiment, iters: int, base_elems: int, op_elems: Optional[int]) -> tuple[float, int, float, float]:
    experiment_name = '{0}-{1}-{2}-{3}-{4}'.format(data_structure.value, experiment.value, iters, base_elems, op_elems)
    print('Running {0}'.format(experiment_name))
    cmd = './dist-newstyle/build/x86_64-linux/ghc-7.11.20220915/tg-0.1.0.0/x/tg/build/tg/tg {0} {1} {2} {3} {4}'.format(data_structure.value, experiment.value, iters, base_elems, op_elems if op_elems is not None else '')
    status = system(cmd)
    # If OS killed (segfault) run again
    while status == 35584:
        print('Got 35584 exit code for {0}. Retrying...'.format(experiment_name))
        print('Running {0}'.format(experiment_name))
        status = system(cmd)

    return get_experiment_result_data()

# Run experiments with several argument valuations
def run_experiments(data_structure: DataStructure, experiment: Experiment) -> list[tuple[float, int, float, float]]:
    (default_iters, default_base_elems, default_op_elems) = get_parameters_default_for_experiment(experiment)
    iters, base_elems, op_elems = get_parameters_for_experiment(experiment)
    csv_data: list[tuple[float, int, float, float]] = []

    print('Gathering data varying number of iterations...')
    for cur_iters in iters:
        experiment_results = run_experiment(data_structure, experiment, cur_iters, default_base_elems, default_op_elems)
        csv_data.append(experiment_results)

    print('Gathering data varying number of base elements...')
    for cur_base_elems in base_elems:
        experiment_results = run_experiment(data_structure, experiment, default_iters, cur_base_elems, default_op_elems)
        csv_data.append(experiment_results)

    print('Gathering data varying number of operator elements...')
    for cur_op_elems in op_elems:
        experiment_results = run_experiment(data_structure, experiment, default_iters, default_base_elems, cur_op_elems)
        csv_data.append(experiment_results)

    return csv_data

def write_csv_data(data_structure: DataStructure, experiment: Experiment, csv_data: list[tuple[float, int, float, float]]):
    makedirs('csv', exist_ok=True)
    with open('csv/{0}-{1}.csv'.format(data_structure.value, experiment.value), 'w') as file:
        file.write('time,alloc,energy_pkg,energy_dram\n')
        lines = ['{0},{1},{2},{3}'.format(line[0], line[1], line[2], line[3]) for line in csv_data]
        file.write('\n'.join(lines))
        file.write('\n')

# Entry point
def main():
    data_structure, experiment = parse_args(argv)
    result_csv = run_experiments(data_structure, experiment)
    write_csv_data(data_structure, experiment, result_csv)

# Only run if executed as main module
if __name__ == '__main__':
    main()
