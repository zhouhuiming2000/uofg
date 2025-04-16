#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Credits: Grigorii Sukhorukov, Macha Nikolski
import os
os.environ["CUDA_VISIBLE_DEVICES"] = ""
os.environ["TF_XLA_FLAGS"] = "--tf_xla_cpu_global_jit"
# loglevel : 0 all printed, 1 I not printed, 2 I and W not printed, 3 nothing printed
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'
import tensorflow as tf
print('number in the brackets corresponds to the GPU being used; empty brackets - no GPU usage')
print(tf.config.list_physical_devices('GPU'))

import fire
import yaml
import tensorflow as tf
import numpy as np
import h5py
import random
from pathlib import Path
import pandas as pd
from joblib import dump, load
from sklearn.utils import shuffle
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import train_test_split
import predict as pr
from utils.batch_loader import BatchLoader, BatchGenerator
from models import model_5, model_7, model_10


def fetch_batches(fragments, fragments_rc, labels, random_seed, batch_size, train_fr):
    batch_size_ = int(batch_size / 2)
    random.seed(a=random_seed)
    n_seqs = int(fragments.shape[0] / 2)
    ind_0 = shuffle(list(range(n_seqs)), random_state=random.randrange(1000000))
    ind_1 = shuffle(list(range(n_seqs, 2 * n_seqs)), random_state=random.randrange(1000000))

    batches = []
    ind_n_, to_add = divmod(batch_size_, 2)
    last_class = [0, 0]  # 只保留两个类别
    for i in range(n_seqs * 2 // batch_size_):
        batch_idx = []
        class_to_add = i % 2
        if to_add == 1:
            ind_n = [ind_n_] * 2
            ind_n[class_to_add] += 1
        else:
            ind_n = [ind_n_] * 2
        batch_idx.extend(ind_0[last_class[0]:last_class[0] + ind_n[0]])
        batch_idx.extend(ind_1[last_class[1]:last_class[1] + ind_n[1]])
        batches.append(batch_idx)
        last_class = [x + y for x, y in zip(last_class, ind_n)]
    assert train_fr < 1.0
    train_batches = batches[:int(len(batches) * train_fr)]
    val_batches = batches[int(len(batches) * train_fr):]
    train_gen = BatchLoader(fragments, fragments_rc, labels, train_batches, rc=True,
                            random_seed=random.randrange(1000000))
    val_gen = BatchLoader(fragments, fragments_rc, labels, val_batches, rc=True, random_seed=random.randrange(1000000))
    return train_gen, val_gen


def train_nn(
        ds_path,
        out_path,
        length,
        epochs=10,
        batch_size=256,
        random_seed=None,
):
    assert Path(out_path).is_dir(), 'out_path was not provided correctly'
    # initializing random generator with the random seed
    random.seed(a=random_seed)
    # callbacks for training of models
    callbacks = [
        tf.keras.callbacks.EarlyStopping(patience=3, monitor='val_loss'),
        tf.keras.callbacks.ReduceLROnPlateau(
            monitor='val_loss', factor=0.1, patience=2, verbose=0, mode='auto',
            min_delta=0.000001, cooldown=3, ),
    ]
    try:
        f = h5py.File(Path(ds_path, f"encoded_train_{length}.hdf5"), "r")
        fragments = f["fragments"]
        fragments_rc = f["fragments_rc"]
        labels = f["labels"]
        # 关键验证点
        assert labels.shape[1] == 2, f"标签维度错误！当前维度：{labels.shape}"
        print(f"验证通过：标签维度为 {labels.shape}")
    except FileNotFoundError:
        raise Exception("dataset was not found. Change ds_path or launch prepare_ds script")
    # print(f'using {random_seed} random_seed in batch generation')
    train_gen, val_gen = fetch_batches(fragments,
                                       fragments_rc,
                                       labels,
                                       random_seed=random_seed,
                                       batch_size=batch_size,
                                       train_fr=0.9)

    models_list = zip(["model_5", "model_7", "model_10"], [model_5, model_7, model_10])

    for model_, model_obj in models_list:
        model = model_obj.model(length)
        model.fit(x=train_gen,
                  validation_data=val_gen,
                  epochs=epochs,
                  callbacks=callbacks,
                  batch_size=batch_size,
                  verbose=2)
        # taking into account validation data
        model.fit(x=val_gen,
                  epochs=1,
                  batch_size=batch_size,
                  verbose=2)
        model.save_weights(Path(out_path, f"{model_}_{length}.h5"))
        print(f'finished training {model_} network')


def subset_df(df, org, thr=0.8, final_df_size=1000):
    """
    Subsets dataset with viral predictions
    For RF classifier to learn from badly predicted viral fragments
    """
    if thr == 1.0:
        df = df.sample(n=final_df_size)
    else:
        df_1 = df.query(f'pred_{org}_5 <= {thr} | pred_{org}_7 <= {thr} | pred_{org}_10 <= {thr}')
        print(df_1.shape[0])
        if df_1.shape[0] < int(final_df_size/2):
            print('too little bad predictions')
            df_1 = df
        df_1 = df_1.sample(n=int(final_df_size/2))
        df_2 = df.query(f'pred_{org}_5 > {thr} & pred_{org}_7 > {thr} & pred_{org}_10 > {thr}')
        if df_2.shape[0] < int(final_df_size/2):
            print('too little good predictions')
            df_2 = df
        df_2 = df_2.sample(n=int(final_df_size/2))
        df = df_1.append(df_2, sort=False)
    return df


def load_ds(df, label, family=None,):
    df = df.drop(['length', 'fragment'], 1)
    df["label"] = label
    if family is not None:
        df["family"] = family
    return df


def merge_ds(path_ds_v, path_ds_pl, fract, rs, family=None,):
    """
    Preprocess predictions by neural network before training RF classifier
    """
    df_vir = load_ds(path_ds_v, label=1, family=family,)
    df_plant = load_ds(path_ds_pl, label=0, family=family,)
    # balancing datasets by downsampling to the size of the smallest dataset
    l_ = min(df_vir.shape[0], df_plant.shape[0])
    df_vir = df_vir.sample(frac=l_ / df_vir.shape[0], random_state=rs)
    df_plant = df_plant.sample(frac=l_ / df_plant.shape[0], random_state=rs)
    # splitting on train and test
    df_vir_train, df_vir_test = train_test_split(df_vir, test_size=fract, shuffle=False)
    df_plant_train, df_plant_test = train_test_split(df_plant, test_size=fract, shuffle=False)
    # 合并数据
    df_train = pd.concat([df_vir_train, df_plant_train])
    df_test = pd.concat([df_vir_test, df_plant_test])
    return df_train, df_test


def fit_clf(df, save_path, rs):
    df_reshuffled = df.sample(frac=1, random_state=rs)
    X = df_reshuffled[["pred_plant_5", "pred_vir_5", "pred_plant_7", "pred_vir_7", "pred_plant_10", "pred_vir_10", ]]
    y = df_reshuffled["label"]
    clf = RandomForestClassifier(max_depth=5, n_estimators=10, max_features=1, max_samples=0.3)
    clf.fit(X, y)
    dump(clf, save_path)
    return clf


def train_rf(nn_weights_path, ds_rf_path, out_path, length, n_cpus, random_seed):
    print('Predictions for test dataset')
    dfs = []
    for org in ['virus', 'plant']:  # 移除'bacteria'
        df = pr.predict_nn(
            ds_path=Path(ds_rf_path, f"seqs_{org}_sampled_{length}_20000.fasta"),
            nn_weights_path=nn_weights_path,
            length=length,
            n_cpus=n_cpus,
            batch_size=256
        )
        dfs.append(df)
    df_v = subset_df(dfs[0], 'vir', thr=0.8)
    df_pl = subset_df(dfs[1], 'plant', thr=1.0)
    print('Training ML classifier')
    df_train, _ = merge_ds(path_ds_v=df_v, path_ds_pl=df_pl, fract=0.2, rs=random_seed)
    _ = fit_clf(df_train, Path(out_path, f"RF_{length}.joblib"), random_seed)


def train(config):
    with open(config, "r") as yamlfile:
        cf = yaml.load(yamlfile, Loader=yaml.FullLoader)

    assert Path(cf["train"]["ds_path"]).exists(), f'{cf["prepare_ds"]["ds_path"]} does not exist'
    Path(cf["train"]["out_path"]).mkdir(parents=True, exist_ok=True)

    for l_ in 500, 1000:
        train_nn(
            ds_path=cf["train"]["ds_path"],
            out_path=cf["train"]["out_path"],
            length=l_,
            epochs=cf["train"]["epochs"],
            random_seed=cf["train"]["random_seed"],
        )
        train_rf(
            nn_weights_path=cf["train"]["out_path"],
            ds_rf_path=cf["train"]["ds_path"],
            out_path=cf["train"]["out_path"],
            length=l_,
            n_cpus=cf["train"]["n_cpus"],
            random_seed=cf["train"]["random_seed"],
        )
        print(f"finished training NN and RF for {l_} fragment size\n")
    print(f"NN and RF weights are stored in {cf['train']['out_path']}")


if __name__ == '__main__':
    fire.Fire(train)
