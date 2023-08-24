from os import system, makedirs
from sys import argv
from enum import Enum
from typing import Optional


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

class DataStructure(Enum):
    SimpleQueue = 'SimpleQueue'
    StandardSet = 'StandardSet'
    AssocList = 'AssocList'
    MinHeap = 'MinHeap'
    StandardMap = 'StandardMap'
    UnbalancedSet = 'UnbalancedSet'
    SplayHeap = 'SplayHeap'

# Create csv folder (if none exists) and dump the experiment results into it
def write_csv_data(data_structure: DataStructure, csv_data: list[tuple[float, float, float, float]]):
    print('Dumping experiment results into csv/...')
    makedirs('csv', exist_ok=True)
    with open('csv/{0}.csv'.format(data_structure.value), 'w') as file:
        file.write('time,alloc,energy_pkg,energy_dram\n')
        lines = ['{0},{1},{2},{3}'.format(line[0], line[1], line[2], line[3]) for line in csv_data]
        file.write('\n'.join(lines))
        file.write('\n')

# Tuple elements are: iterations, base_elems, operation elems
def get_parameters_default_for_experiment(experiment: Experiment, base_elems_reducing_factor) -> tuple[int, int, Optional[int]]:
    match experiment:
        case Experiment.Add:
            return (1, 100000 // base_elems_reducing_factor, 100000)
        case Experiment.AddAll:
            return (1000, 100000 // base_elems_reducing_factor, 1000)
        case Experiment.Clear:
            return (1, 100000 // base_elems_reducing_factor, None)
        case Experiment.Contains:
            return (1000, 100000 // base_elems_reducing_factor, None)
        case Experiment.ContainsAll:
            return (5000, 100000 // base_elems_reducing_factor, 1000)
        case Experiment.Iterator:
            return (1, 100000 // base_elems_reducing_factor, None)
        case Experiment.Remove:
            return (10000, 100000 // base_elems_reducing_factor, None)
        case Experiment.RemoveAll:
            return (10, 100000 // base_elems_reducing_factor, 1000)
        case Experiment.RetainAll:
            return (10, 100000 // base_elems_reducing_factor, 1000)
        case Experiment.ToList:
            return (5000, 100000 // base_elems_reducing_factor, None)

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
    cmd = './dist-newstyle/build/x86_64-linux/ghc-7.11.20221006/tg-0.1.0.0/x/tg/build/tg/tg {0} {1} {2} {3} {4}'.format(data_structure.value, experiment.value, iters, base_elems, op_elems if op_elems is not None else '')
    status = system(cmd)
    # If OS killed (segfault) run again
    while status == 35584:
        print('Got 35584 exit code for {0}. Retrying...'.format(experiment_name))
        print('Running {0}'.format(experiment_name))
        status = system(cmd)

    return get_experiment_result_data()

def parse_args(args):
    valid_commands = []
    if(len(args) == 1):
        raise ValueError('Please provide one of the following commands as an argument: {}'.format(valid_commands))

def main():

    data_strucutres = [DataStructure.SplayHeap, DataStructure.StandardMap, DataStructure.UnbalancedSet]
    for data_structure in data_strucutres:
        csv_data: list[tuple[float, float, float, float]] = []

        for e in Experiment:
            default_parameters = get_parameters_default_for_experiment(e, 1000)
            if default_parameters != None:
                (iters, base_elems, op_elems) = default_parameters
                csv_data.append(run_experiment(data_structure, e, iters, base_elems, op_elems))
            else:
                print('Default parameter cant be none')

        write_csv_data(data_structure, csv_data)

if __name__ == '__main__':
    main()