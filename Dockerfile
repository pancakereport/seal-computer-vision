FROM nvcr.io/nvidia/tensorflow:20.09-tf2-py3

ARG DEBIAN_FRONTEND="noninteractive" 
WORKDIR /app

RUN apt-get update && apt-get install -y --no-install-recommends \
    git \
    wget \
    python3-opencv \
    && rm -rf /var/lib/apt/lists/*

RUN pip3 install --no-cache-dir --upgrade pip && \
    pip3 install --no-cache-dir \
    jupyter \
    jupyterlab \
    keras==2.4.0 \
    tensorflow==2.4.0 \
    keras_retinanet \
    matplotlib \
    numpy \
    progressbar2 \
    Pillow \
    pandas

RUN git clone https://github.com/fizyr/keras-retinanet.git && \
    git clone https://github.com/martinzlocha/anchor-optimization.git && \
    git clone https://github.com/pancakereport/seal-computer-vision.git

RUN mkdir -p seal-computer-vision/Data/harbor-seals/models && \
    mkdir -p seal-computer-vision/Data/elephant-seals/models && \
    wget -q https://github.com/fizyr/keras-retinanet/releases/download/0.5.1/resnet50_coco_best_v2.1.0.h5 && \
    wget -q -P seal-computer-vision/Data/harbor-seals/models https://harbor-seal-models.s3.amazonaws.com/model_no_other.h5 && \
    wget -q -P seal-computer-vision/Data/harbor-seals/models https://harbor-seal-models.s3.amazonaws.com/model_other.h5 && \
    wget -q -P seal-computer-vision/Data/elephant-seals/models https://elephant-seal-models.s3.amazonaws.com/model_no_other.h5 && \
    wget -q -P seal-computer-vision/Data/elephant-seals/models https://elephant-seal-models.s3.amazonaws.com/model_other.h5


CMD ["python3", "keras-retinanet/setup.py", "build_ext", "--inplace"]
