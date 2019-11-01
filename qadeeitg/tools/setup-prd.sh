#!/usr/bin/sh
# Shellscript: setup-prd.sh
# Description: General Production Setup Script
#
# %Z% SCCS-Id:%P% %I% %G% %U% tkl
#
# REVISION: EE   LAST MODIFIED: 16 Jul 2015  BY: HP/TKL

# Setup Database Backup Directory structure

mkdir /opt/shared/backup/csi_prd_css
mkdir /opt/shared/backup/csi_prd_cus
mkdir /opt/shared/backup/is_prd_com
mkdir /opt/shared/backup/is_prd_main
mkdir /opt/shared/backup/qad_prd_adm
mkdir /opt/shared/backup/qad_prd_alt
mkdir /opt/shared/backup/qad_prd_bpm
mkdir /opt/shared/backup/qad_prd_cus
mkdir /opt/shared/backup/qad_prd_hlp
mkdir /opt/shared/backup/qad_prd_mfg
mkdir /opt/shared/backup/qad_prd_qra
mkdir /opt/shared/backup/qad_prd_qxe
mkdir /opt/shared/backup/qad_prd_qxo
mkdir /opt/shared/backup/sys_prd_pm

find /opt/shared/backup -type d | xargs -i chown qadeeitg:qad {}
find /opt/shared/backup -type d | xargs -i chmod 755 {}

# Setup Database Directory structure

mkdir /opt/shared/dbeE/csi_prd_css
mkdir /opt/shared/dbeE/csi_prd_css/ai
mkdir /opt/shared/dbeE/csi_prd_css/bi
mkdir /opt/shared/dbeE/csi_prd_css/db
mkdir /opt/shared/dbeE/csi_prd_cus
mkdir /opt/shared/dbeE/csi_prd_cus/ai
mkdir /opt/shared/dbeE/csi_prd_cus/bi
mkdir /opt/shared/dbeE/csi_prd_cus/db
mkdir /opt/shared/dbeE/is_prd_com
mkdir /opt/shared/dbeE/is_prd_com/ai
mkdir /opt/shared/dbeE/is_prd_com/bi
mkdir /opt/shared/dbeE/is_prd_com/db
mkdir /opt/shared/dbeE/is_prd_main
mkdir /opt/shared/dbeE/is_prd_main/ai
mkdir /opt/shared/dbeE/is_prd_main/bi
mkdir /opt/shared/dbeE/is_prd_main/db
mkdir /opt/shared/dbeE/qad_prd_adm
mkdir /opt/shared/dbeE/qad_prd_adm/ai
mkdir /opt/shared/dbeE/qad_prd_adm/bi
mkdir /opt/shared/dbeE/qad_prd_adm/db
mkdir /opt/shared/dbeE/qad_prd_alt
mkdir /opt/shared/dbeE/qad_prd_alt/ai
mkdir /opt/shared/dbeE/qad_prd_alt/bi
mkdir /opt/shared/dbeE/qad_prd_alt/db
mkdir /opt/shared/dbeE/qad_prd_bpm
mkdir /opt/shared/dbeE/qad_prd_bpm/ai
mkdir /opt/shared/dbeE/qad_prd_bpm/bi
mkdir /opt/shared/dbeE/qad_prd_bpm/db
mkdir /opt/shared/dbeE/qad_prd_cus
mkdir /opt/shared/dbeE/qad_prd_cus/ai
mkdir /opt/shared/dbeE/qad_prd_cus/bi
mkdir /opt/shared/dbeE/qad_prd_cus/db
mkdir /opt/shared/dbeE/qad_prd_hlp
mkdir /opt/shared/dbeE/qad_prd_hlp/ai
mkdir /opt/shared/dbeE/qad_prd_hlp/bi
mkdir /opt/shared/dbeE/qad_prd_hlp/db
mkdir /opt/shared/dbeE/qad_prd_mfg
mkdir /opt/shared/dbeE/qad_prd_mfg/ai
mkdir /opt/shared/dbeE/qad_prd_mfg/bi
mkdir /opt/shared/dbeE/qad_prd_mfg/db
mkdir /opt/shared/dbeE/qad_prd_qra
mkdir /opt/shared/dbeE/qad_prd_qra/ai
mkdir /opt/shared/dbeE/qad_prd_qra/bi
mkdir /opt/shared/dbeE/qad_prd_qra/db
mkdir /opt/shared/dbeE/qad_prd_qxe
mkdir /opt/shared/dbeE/qad_prd_qxe/ai
mkdir /opt/shared/dbeE/qad_prd_qxe/bi
mkdir /opt/shared/dbeE/qad_prd_qxe/db
mkdir /opt/shared/dbeE/qad_prd_qxo
mkdir /opt/shared/dbeE/qad_prd_qxo/ai
mkdir /opt/shared/dbeE/qad_prd_qxo/bi
mkdir /opt/shared/dbeE/qad_prd_qxo/db
mkdir /opt/shared/dbeE/sys_prd_pm
mkdir /opt/shared/dbeE/sys_prd_pm/ai
mkdir /opt/shared/dbeE/sys_prd_pm/bi
mkdir /opt/shared/dbeE/sys_prd_pm/db

find /opt/shared/dbeE -type d | xargs -i chown qadeeitg:qad {}
find /opt/shared/dbeE -type d | xargs -i chmod 755 {}


# Setup Database After Image Directory structure

mkdir /opt/shared/aimaint/csi_prd_css
mkdir /opt/shared/aimaint/csi_prd_css/aiarchive
mkdir /opt/shared/aimaint/csi_prd_css/aidump
mkdir /opt/shared/aimaint/csi_prd_css/aistage
mkdir /opt/shared/aimaint/csi_prd_cus
mkdir /opt/shared/aimaint/csi_prd_cus/aiarchive
mkdir /opt/shared/aimaint/csi_prd_cus/aidump
mkdir /opt/shared/aimaint/csi_prd_cus/aistage
mkdir /opt/shared/aimaint/is_prd_com
mkdir /opt/shared/aimaint/is_prd_com/aiarchive
mkdir /opt/shared/aimaint/is_prd_com/aidump
mkdir /opt/shared/aimaint/is_prd_com/aistage
mkdir /opt/shared/aimaint/is_prd_main
mkdir /opt/shared/aimaint/is_prd_main/aiarchive
mkdir /opt/shared/aimaint/is_prd_main/aidump
mkdir /opt/shared/aimaint/is_prd_main/aistage
mkdir /opt/shared/aimaint/qad_prd_adm
mkdir /opt/shared/aimaint/qad_prd_adm/aiarchive
mkdir /opt/shared/aimaint/qad_prd_adm/aidump
mkdir /opt/shared/aimaint/qad_prd_adm/aistage
mkdir /opt/shared/aimaint/qad_prd_alt
mkdir /opt/shared/aimaint/qad_prd_alt/aiarchive
mkdir /opt/shared/aimaint/qad_prd_alt/aidump
mkdir /opt/shared/aimaint/qad_prd_alt/aistage
mkdir /opt/shared/aimaint/qad_prd_bpm
mkdir /opt/shared/aimaint/qad_prd_bpm/aiarchive
mkdir /opt/shared/aimaint/qad_prd_bpm/aidump
mkdir /opt/shared/aimaint/qad_prd_bpm/aistage
mkdir /opt/shared/aimaint/qad_prd_cus
mkdir /opt/shared/aimaint/qad_prd_cus/aiarchive
mkdir /opt/shared/aimaint/qad_prd_cus/aidump
mkdir /opt/shared/aimaint/qad_prd_cus/aistage
mkdir /opt/shared/aimaint/qad_prd_hlp
mkdir /opt/shared/aimaint/qad_prd_hlp/aiarchive
mkdir /opt/shared/aimaint/qad_prd_hlp/aidump
mkdir /opt/shared/aimaint/qad_prd_hlp/aistage
mkdir /opt/shared/aimaint/qad_prd_mfg
mkdir /opt/shared/aimaint/qad_prd_mfg/aiarchive
mkdir /opt/shared/aimaint/qad_prd_mfg/aidump
mkdir /opt/shared/aimaint/qad_prd_mfg/aistage
mkdir /opt/shared/aimaint/qad_prd_qra
mkdir /opt/shared/aimaint/qad_prd_qra/aiarchive
mkdir /opt/shared/aimaint/qad_prd_qra/aidump
mkdir /opt/shared/aimaint/qad_prd_qra/aistage
mkdir /opt/shared/aimaint/qad_prd_qxe
mkdir /opt/shared/aimaint/qad_prd_qxe/aiarchive
mkdir /opt/shared/aimaint/qad_prd_qxe/aidump
mkdir /opt/shared/aimaint/qad_prd_qxe/aistage
mkdir /opt/shared/aimaint/qad_prd_qxo
mkdir /opt/shared/aimaint/qad_prd_qxo/aiarchive
mkdir /opt/shared/aimaint/qad_prd_qxo/aidump
mkdir /opt/shared/aimaint/qad_prd_qxo/aistage
mkdir /opt/shared/aimaint/sys_prd_pm
mkdir /opt/shared/aimaint/sys_prd_pm/aiarchive
mkdir /opt/shared/aimaint/sys_prd_pm/aidump
mkdir /opt/shared/aimaint/sys_prd_pm/aistage

find /opt/shared/aimaint -type d | xargs -i chown qadeeitg:qad {}
find /opt/shared/aimaint -type d | xargs -i chmod 755 {}


