# 上传本仓库到 GitHub 的步骤

## 方式一：网页拖拽（最简单，推荐）

1. 下载 `github_repo.zip` 并解压，得到 `postop-critical-care-triage/` 文件夹
2. 登录 GitHub → 右上角 **"+"** → **New repository**
   - Repository name: `postop-critical-care-triage`（或您喜欢的名字）
   - 建议先设为 **Private**，投稿接收后再转 Public
   - **不要**勾选 "Add a README"（仓库里已有）
3. 创建后，在仓库页面点 **"uploading an existing file"** 链接
   （或之后点 **Add file → Upload files**）
4. 把解压出的 `postop-critical-care-triage` **文件夹整个拖进上传区域**
   ——GitHub 会自动保留 R/、python/、audit/ 等子目录结构
5. 等全部 87 个文件上传完，页面底部点 **Commit changes**，完成

说明：网页上传单文件上限 25 MB，本仓库全是文本代码（共约 600 KB），远低于限制。

## 方式二：命令行（熟悉 git 的话）

```bash
cd postop-critical-care-triage
git init
git add .
git commit -m "Analysis code for postoperative critical care triage study"
git branch -M main
git remote add origin git@github.com:<您的用户名>/postop-critical-care-triage.git
git push -u origin main
```

## 上传前请检查

- `LICENSE` 版权行已设为 Dong Guo（如需加其他作者可再编辑）
- `README.md` 中的 Citation 部分，接收后补充正式引用
- 确认仓库中**不含**任何患者级数据或模型权重（当前已排除 `.rds`/`.pt`）

## （推荐）用 Zenodo 存档获取 DOI

1. 登录 <https://zenodo.org/>（可用 GitHub 账号）
2. 在 Settings → GitHub 中开启该仓库的同步
3. 在 GitHub 上创建一个 Release（如 `v1.0.0`）
4. Zenodo 会自动存档该 Release 并分配 DOI
5. 把 DOI 填入手稿的 Code Availability 声明

## Code Availability 声明（可直接用于手稿）

> The analysis code is available at
> https://github.com/<您的用户名>/postop-critical-care-triage (archived on Zenodo,
> DOI: 待填). VitalDB is publicly available at https://vitaldb.net; INSPIRE is
> available via PhysioNet under a data-use agreement; MOVER is available from the
> MOVER project (UC Irvine) under its data-use agreement. No patient-level data
> are redistributed.
