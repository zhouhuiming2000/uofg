import streamlit as st
import requests
import joblib
import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
import requests

# ============ 1. 加载模型和Scaler ============
model = joblib.load("model_rf_all.pkl")  # 训练好的随机森林模型
scaler = joblib.load("scaler_all.pkl")   # 训练时的Scaler
columns_order = joblib.load("predictors_columns_order.pkl")  # 模型训练时的列顺序

# ============ 2. 获取海拔信息函数 ============
def get_single_elevation(lat, lon):
    payload = {
        "locations": [{"latitude": lat, "longitude": lon}]
    }
    url = "https://api.open-elevation.com/api/v1/lookup"
    headers = {
        'Accept': 'application/json',
        'Content-Type': 'application/json'
    }
    try:
        response = requests.post(url, json=payload, headers=headers)
        response.raise_for_status()
        return response.json()['results'][0]['elevation']
    except requests.exceptions.RequestException as e:
        st.warning(f"Elevation API Error: {e}")
        return 0

# ============ 3. Streamlit 页面设置 ============
st.set_page_config(page_title="Corn Maturity Group Recommender", layout="centered")
st.title("🌽 Optimal Maturity Group Predictor")

# ============ 4. 用户输入界面 ============
with st.form("prediction_form"):
    st.subheader("📍 Location")
    lat = st.number_input("Latitude", value=40.0, format="%.6f")
    lon = st.number_input("Longitude", value=-86.0, format="%.6f")

    st.subheader("🌱 Field Management")
    tillage = st.selectbox("Tillage", ['Conventional', 'No-Till', 'Minimum', 'Strip-Till', 'Ridge-Till'])
    previous_crop = st.selectbox("Previous Crop", ['Soybean', 'Wheat', 'Corn', 'Other'])
    trait = st.selectbox("Trait", ['SSRIB', 'TRERIB', 'VT2PRIB', 'AM', 'Other', 'QROME'])
    soil_texture = st.selectbox("Soil Texture", ['Sandy Loam', 'Clay Loam', 'Clay', 'Sand', 'Loam', 'Loamy Sand',
        'Sandy Clay Loam', 'Silt Clay Loam', 'Silt Loam', 'Silt Clay', 'Sandy Clay',
        'Silty Clay Loam', 'Silty Loam', 'Silty Clay'])

    st.subheader("⏳ Maturity Group Range")
    maturity_min = st.number_input("Min Maturity Group", value=95, step=1)
    maturity_max = st.number_input("Max Maturity Group", value=115, step=1)

    submitted = st.form_submit_button("Predict Best Maturity Group")

# ============ 5. 执行预测逻辑 ============
if submitted:
    if not (-90 <= lat <= 90 and -180 <= lon <= 180):
        st.error("❌ Invalid Latitude or Longitude.")
    elif maturity_min > maturity_max:
        st.error("❌ Min maturity group must be less than or equal to max.")
    else:
        st.info("Fetching elevation data...")
        elevation = get_single_elevation(lat, lon)
        st.success(f"Elevation at this point: {elevation} meters")

        # 构建 candidate dataframe
        candidate_rows = []
        for mg in range(int(maturity_min), int(maturity_max)+1):
            candidate_rows.append({
                "Latitude": round(lat, 2),
                "Longitude": round(lon, 2),
                "Elevation": elevation,
                "Tillage": tillage,
                "Previous Crop": previous_crop,
                "Trait": trait,
                "Soil Texture": soil_texture,
                "Relative Maturity": mg
            })

        df_input = pd.DataFrame(candidate_rows)

        # ============ 6. 编码 & 处理一致性 ============
        # 编码方式与你提供的保持一致
        df_encoded = pd.get_dummies(df_input, drop_first=True, 
                                    columns=["Previous Crop", "Trait"], dtype=int)
        df_encoded["Latitude"] = round(df_encoded["Latitude"], 2)
        df_encoded["Longitude"] = round(df_encoded["Longitude"], 2)

        # 删除不使用的列
        df_encoded = df_encoded.drop(columns=["Tillage", "Soil Texture"])

        # 保证列顺序一致
        for col in columns_order:
            if col not in df_encoded.columns:
                df_encoded[col] = 0  # 补上缺失的列（比如 one-hot 中不出现的）
        df_encoded = df_encoded[columns_order]

        # 标准化
        df_scaled = scaler.transform(df_encoded)

        # ============ 7. 模型预测 ============
        predictions = model.predict(df_scaled)
        df_input["Predicted Yield"] = predictions

        best_row = df_input.loc[df_input["Predicted Yield"].idxmax()]
        st.subheader("🎯 Best Maturity Group Recommendation")
        st.markdown(f"""
        - ✅ **Recommended Maturity Group**: `{int(best_row['Relative Maturity'])}`
        - 📈 **Predicted Yield**: `{best_row['Predicted Yield']:.2f}`
        """)

        st.dataframe(df_input[["Relative Maturity", "Predicted Yield"]].round(2).reset_index(drop=True))
