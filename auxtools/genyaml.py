#!/usr/bin/env python3

import argparse
import yaml
import random

parser = argparse.ArgumentParser(
    description='a tool to generate yaml files for testing yaml-erd')
parser.add_argument('--n_entities', type=int,
                    help='the number of generated entities', required=True)
parser.add_argument('--relation_prob', type=float,
                    help='the probability of existence for each relation', required=True)

args = parser.parse_args()
n_entities = args.n_entities
rel_prob = args.relation_prob


def mkEntity(n):
    relations = []
    for i in range(n_entities):
        if n != i and random.random() <= rel_prob:
            relations.append('(id) -- Entity{i}(id)'.format(i=i))
    return {
        'struct': {'id': 'int', 'data': 'name'},
        'relations': relations
    }


entities = {'Entity{i}'.format(i=i): mkEntity(i) for i in range(n_entities)}

data = {'schema': entities}

f = open('output.yaml', 'w')
f.write(yaml.dump(data, default_flow_style=False))
