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

class ExperimentParameter(Enum):
    Iterations = 'Iterations'
    BaseElements = 'BaseElements'
    OperationElements = 'OperationElements'
supported_experiment_parameters = [e.value for e in ExperimentParameter]

# Experiments default parameters (as defined by the research table)
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

# Reads tg.prof (profiler output) file and extract time, allocation, processor and dram energy data
def get_experiment_result_data() -> tuple[float, float, float, float]:
    with open('tg.prof', 'r', encoding='ASCII') as prof_file:
        lines = prof_file.readlines()
        total_time = float([line for line in lines if 'total time' in line][0].split('=')[1].split('secs')[0])
        total_alloc = int([line for line in lines if 'total alloc' in line][0].split('=')[1].split('bytes')[0].replace(',', ''))
        total_energy_pkg = float([line for line in lines if 'total energy pkg' in line][0].split('=')[1].split('joules')[0])
        total_energy_dram = float([line for line in lines if 'total energy dram' in line][0].split('=')[1].split('joules')[0])
        experiment_lines = [line for line in lines if 'runNExperimentFunction' in line]
        # if the experiment function was a major cost centre, we need to access the second line because it will show up first in the major report
        experiment_line_index = 0 if len(experiment_lines) == 1 else 1
        experiment_line = list(filter(lambda v: v, experiment_lines[experiment_line_index].split(' ')))
        # Percentages that the experiment took
        exp_time_perc, exp_alloc_perc, exp_energy_pkg_perc, exp_energy_dram_perc = float(experiment_line[8])/100, float(experiment_line[9])/100, float(experiment_line[10])/100, float(experiment_line[11][:-1])/100
        return (total_time*exp_time_perc, total_alloc*exp_alloc_perc/(1024*1024), total_energy_pkg*exp_energy_pkg_perc, total_energy_dram*exp_energy_dram_perc)

# Execute one experiment given a data structure, experiment type (operation) number of iterations, number of base elems, number of operator elems
def run_experiment(data_structure: DataStructure, experiment: Experiment, iters: int, base_elems: int, op_elems: Optional[int]) -> tuple[float, float, float, float]:
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

# Run experiments varying the experiment parameter from [initial_value, final_value]
def run_experiments(data_structure: DataStructure, experiment: Experiment, experiment_parameter: ExperimentParameter, steps: int, initial_value: int, final_value: int) -> list[tuple[float, float, float, float]]:
    (default_iters, default_base_elems, default_op_elems) = get_parameters_default_for_experiment(experiment)
    csv_data: list[tuple[float, float, float, float]] = []

    print('Starting to vary {0} from {1} to {2} in {3} steps'.format(experiment_parameter.value, initial_value, final_value, steps))
    current_value = initial_value
    step_size = (final_value - current_value)//(steps-1)
    while steps > 0:
        match experiment_parameter:
            case ExperimentParameter.Iterations:
                experiment_results = run_experiment(data_structure, experiment, current_value, default_base_elems, default_op_elems)
            case ExperimentParameter.BaseElements:
                experiment_results = run_experiment(data_structure, experiment, default_iters, current_value, default_op_elems)
            case ExperimentParameter.OperationElements:
                experiment_results = run_experiment(data_structure, experiment, default_iters, default_base_elems, current_value)

        csv_data.append(experiment_results)
        current_value += step_size
        steps -= 1

    return csv_data

# Create csv folder (if none exists) and dump the experiment results into it
def write_csv_data(data_structure: DataStructure, experiment: Experiment, csv_data: list[tuple[float, float, float, float]]):
    print('Dumping experiment results into csv/...')
    makedirs('csv', exist_ok=True)
    with open('csv/{0}-{1}.csv'.format(data_structure.value, experiment.value), 'w') as file:
        file.write('time,alloc,energy_pkg,energy_dram\n')
        lines = ['{0},{1},{2},{3}'.format(line[0], line[1], line[2], line[3]) for line in csv_data]
        file.write('\n'.join(lines))
        file.write('\n')

# Returns validated data structure and experiment
def parse_args(args: list[str]) -> tuple[DataStructure, Experiment]:
    # First argument is the script name
    if len(args) < 6:
        raise ValueError('Not enough arguments. Specify data structure, experiment, experiment parameter (to vary), step size, initial param value, final param value.')

    if args[1] not in supported_data_structures:
        raise ValueError('Invalid data structure. Pick one of: {0}'.format(supported_data_structures))

    if args[2] not in supported_experiments:
        raise ValueError('Invalid experiment. Pick one of: {0}'.format(supported_experiments))

    if args[3] not in supported_experiment_parameters:
        raise ValueError('Invalid experiment parameter. Pick one of: {0}'.format(supported_experiment_parameters))

    try:
        steps = int(args[4])
    except ValueError:
        raise ValueError('Invalid experiment steps. Must be int, given: {0}'.format(args[4]))

    try:
        initial_param_value = int(args[5])
    except ValueError:
        raise ValueError('Invalid experiment initial param value. Must be int, given: {0}'.format(args[5]))

    try:
        final_param_value = int(args[6])
    except ValueError:
        raise ValueError('Invalid experiment final param value. Must be int, given: {0}'.format(args[6]))

    return DataStructure[args[1]], Experiment[args[2]], ExperimentParameter[args[3]], steps, initial_param_value, final_param_value

# Entry point
def main():
    data_structure, experiment, experiment_param, steps, initial_param_value, final_param_value = parse_args(argv)
    result_csv = run_experiments(data_structure, experiment, experiment_param, steps, initial_param_value, final_param_value)
    write_csv_data(data_structure, experiment, result_csv)

# Only run if executed as main module
if __name__ == '__main__':
    main()
