#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Credits: Grigorii Sukhorukov, Macha Nikolski
import os
os.environ["CUDA_VISIBLE_DEVICES"] = ""
os.environ["TF_XLA_FLAGS"] = "--tf_xla_cpu_global_jit"
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'
import fire
import yaml
import tensorflow as tf
import numpy as np
from Bio import SeqIO
import pandas as pd
import ray
from utils import preprocess as pp
from pathlib import Path
from models import model_5, model_7, model_10
from joblib import load
import psutil


def predict_nn(ds_path, nn_weights_path, length, n_cpus=1, batch_size=256):
    """
    Breaks down contigs into fragments
    and uses pretrained neural networks to give predictions for fragments
    """
    try:
        pid = psutil.Process(os.getpid())
        pid.cpu_affinity(range(n_cpus))
    except AttributeError:
        print("CPU affinity setting skipped")

    try:
        seqs_ = list(SeqIO.parse(ds_path, "fasta"))
    except FileNotFoundError:
        raise Exception("Test dataset not found")

    out_table = {
        "id": [],
        "length": [],
        "fragment": [],
        "pred_plant_5": [],
        "pred_vir_5": [],
        "pred_plant_7": [],
        "pred_vir_7": [],
        "pred_plant_10": [],
        "pred_vir_10": [],
    }
    
    if not seqs_:
        raise ValueError("All sequences were smaller than model length")
        
    test_fragments = []
    test_fragments_rc = []
    ray.init(num_cpus=n_cpus, include_dashboard=False)
    
    for seq in seqs_:
        fragments_, fragments_rc, _ = pp.fragmenting([seq], length, max_gap=0.8,
                                                     sl_wind_step=int(length/2))
        test_fragments.extend(fragments_)
        test_fragments_rc.extend(fragments_rc)
        for j in range(len(fragments_)):
            out_table["id"].append(seq.id)
            out_table["length"].append(len(seq.seq))
            out_table["fragment"].append(j)

    # One-hot encoding
    it = pp.chunks(test_fragments, len(test_fragments)//n_cpus+1)
    test_encoded = np.concatenate(ray.get([pp.one_hot_encode.remote(s) for s in it]))
    it = pp.chunks(test_fragments_rc, len(test_fragments_rc)//n_cpus+1)
    test_encoded_rc = np.concatenate(ray.get([pp.one_hot_encode.remote(s) for s in it]))
    ray.shutdown()

    # Prediction
    for model, s in zip([model_5.model(length), model_7.model(length), model_10.model(length)], [5,7,10]):
        model.load_weights(Path(nn_weights_path, f"model_{s}_{length}.h5"))
        prediction = model.predict([test_encoded, test_encoded_rc], batch_size=batch_size)
        # 只保留virus和plant的预测
        out_table[f"pred_plant_{s}"].extend(list(prediction[..., 0]))
        out_table[f"pred_vir_{s}"].extend(list(prediction[..., 1]))

    return pd.DataFrame(out_table).round(3)


def predict_rf(df, rf_weights_path, length):
    """
    Random Forest prediction (二分类版本)
    """
    clf = load(Path(rf_weights_path, f"RF_{length}.joblib"))
    X = df[["pred_plant_5", "pred_vir_5", "pred_plant_7", "pred_vir_7", "pred_plant_10", "pred_vir_10"]]
    
    # 二分类映射
    mapping = {0: "plant", 1: "virus"}  # 修改为二分类
    df["RF_decision"] = np.vectorize(mapping.get)(clf.predict(X))
    
    # 概率预测
    prob_classes = clf.predict_proba(X)
    df["RF_pred_plant"] = prob_classes[..., 0]
    df["RF_pred_vir"] = prob_classes[..., 1]
    
    return df


def predict_contigs(df):
    """
    Final contig prediction (二分类版本)
    """
    # Group by decision
    df = (
        df.groupby(["id", "length", 'RF_decision'], sort=False)
        .size()
        .unstack(fill_value=0)
    )
    
    # Ensure columns exist
    for col in ['virus', 'plant']:
        if col not in df.columns:
            df[col] = 0
            
    df = df.reset_index().reindex(['id', 'length', 'virus', 'plant'], axis=1).fillna(0)
    
    # 二分类决策逻辑
    conditions = [
        (df['virus'] > df['plant']),
        (df['plant'] >= df['virus'])
    ]
    choices = ['virus', 'plant']
    df['decision'] = np.select(conditions, choices, default='plant')
    
    # 重命名列
    df = df.rename(columns={
        'virus': '# viral fragments',
        'plant': '# plant fragments'
    })
    
    # 计算病毒片段比例
    df['# viral / # total'] = (
        df['# viral fragments'] / 
        (df['# viral fragments'] + df['# plant fragments'])
    ).round(3)
    
    return df[['id', 'length', '# viral fragments', '# plant fragments', 'decision', '# viral / # total']]


def predict(config):
    """
    Main prediction function
    """
    with open(config, "r") as yamlfile:
        cf = yaml.load(yamlfile, Loader=yaml.FullLoader)

    # 输入验证
    test_ds = cf["predict"]["test_ds"]
    if isinstance(test_ds, str):
        test_ds = [test_ds]
    assert all(Path(t).exists() for t in test_ds), "Test dataset not found"
    
    Path(cf['predict']['out_path']).mkdir(parents=True, exist_ok=True)

    for ts in test_ds:
        dfs_fr, dfs_cont = [], []
        for l_ in (500, 1000):
            print(f'Predicting {Path(ts).name} with fragment length {l_}')
            
            # NN预测
            df = predict_nn(
                ds_path=ts,
                nn_weights_path=cf["predict"]["weights"],
                length=l_,
                n_cpus=cf["predict"]["n_cpus"],
            )
            
            # RF分类
            df = predict_rf(df, cf["predict"]["weights"], l_)
            dfs_fr.append(df.round(3))
            
            # Contig级别预测
            df_contig = predict_contigs(df.round(3))
            dfs_cont.append(df_contig)
            print(f'Length {l_} prediction completed')

        # 合并结果
        limit = cf["predict"]["limit"]
        df_500 = dfs_fr[0][(dfs_fr[0]['length'] >= limit) & (dfs_fr[0]['length'] < 1500)]
        df_1000 = dfs_fr[1][(dfs_fr[1]['length'] >= 1500)]
        df_final = pd.concat([df_1000, df_500], ignore_index=True)
        
        # 保存结果
        pred_fr_path = Path(cf['predict']['out_path'], f"{Path(ts).stem}_predicted_fragments.csv")
        df_final.to_csv(pred_fr_path, index=False)
        
        # Contig级别结果
        df_cont = pd.concat(dfs_cont, ignore_index=True)
        pred_contig_path = Path(cf['predict']['out_path'], f"{Path(ts).stem}_predicted.csv")
        df_cont.to_csv(pred_contig_path, index=False)
        
        # 输出病毒序列
        if cf["predict"]["return_viral"]:
            viral_ids = df_cont[df_cont["decision"] == "virus"]["id"].tolist()
            seqs_ = list(SeqIO.parse(ts, "fasta"))
            viral_seqs = [s for s in seqs_ if s.id in viral_ids]
            viral_path = Path(cf['predict']['out_path'], f"{Path(ts).stem}_viral.fasta")
            SeqIO.write(viral_seqs, viral_path, 'fasta')


if __name__ == '__main__':
    fire.Fire(predict)