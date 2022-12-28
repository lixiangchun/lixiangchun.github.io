
function createImageVisAttrs(keyName, imageFolderUrl, imageWidth, imageHeight) {
    var imageVisAttrs = {
          key: keyName,
          tileSource: {
            Image: {
              xmlns: 'http://schemas.microsoft.com/deepzoom/2008',
              Url: imageFolderUrl, 
              Format: 'jpeg', Overlap: '1', TileSize: '254',
              Size: {
                Width:  String(imageWidth),
                Height: String(imageHeight),
              }
            }
          },
          shown: true
        };
    return imageVisAttrs;
}

function getDefaultSlide() {
    const originImageUrl = 'dzi/C3L-00601-26_orig_1.tif/slide_files/';
    const head0ImageUrl = 'dzi/C3L-00601-26_0.8_roi_0_blur_1_rs_1_bc_0_a_0.4_l_1_bi_0_-1.0_WIT_head_0.tif/slide_files/';
    const head1ImageUrl = 'dzi/C3L-00601-26_0.8_roi_0_blur_1_rs_1_bc_0_a_0.4_l_1_bi_0_-1.0_WIT_head_1.tif/slide_files/';
    const head2ImageUrl = 'dzi/C3L-00601-26_0.8_roi_0_blur_1_rs_1_bc_0_a_0.4_l_1_bi_0_-1.0_WIT_head_2.tif/slide_files/';
    const head3ImageUrl = 'dzi/C3L-00601-26_0.8_roi_0_blur_1_rs_1_bc_0_a_0.4_l_1_bi_0_-1.0_WIT_head_3.tif/slide_files/';
    const AbMILImageUrl = 'dzi/C3L-00601-26_0.8_roi_0_blur_1_rs_1_bc_0_a_0.4_l_1_bi_0_-1.0_AbMIL.tif/slide_files/';

    const width = '5477';
    const height = '5154';

    var originImage  = createImageVisAttrs('original_image',      originImageUrl, width, height);
    var head0AttnMap = createImageVisAttrs('head0_attention_map', head0ImageUrl,  width, height);
    var head1AttnMap = createImageVisAttrs('head1_attention_map', head1ImageUrl,  width, height);
    var head2AttnMap = createImageVisAttrs('head2_attention_map', head2ImageUrl,  width, height);
    var head3AttnMap = createImageVisAttrs('head3_attention_map', head3ImageUrl,  width, height);
    var AbMILAttnMap  = createImageVisAttrs('AbMIL_attention_map',  AbMILImageUrl,   width, height);

	/**
    var viewer = new CurtainSyncViewer({
    container: document.querySelector('#viewer'),
      images: [
        originImage, head0AttnMap, head1AttnMap, head2AttnMap, head3AttnMap, AbMILAttnMap
      ],
    });
	**/

    return [originImage, head0AttnMap, head1AttnMap, head2AttnMap, head3AttnMap, AbMILAttnMap];
}

function getSelectedSlide(info, ID) {

    var originImage  = createImageVisAttrs('original_image',      info[ID]['HE'].Url,     info[ID].width, info[ID].height);
    var head0AttnMap = createImageVisAttrs('head0_attention_map', info[ID]['Head0'].Url,  info[ID].width, info[ID].height);
    var head1AttnMap = createImageVisAttrs('head1_attention_map', info[ID]['Head1'].Url,  info[ID].width, info[ID].height);
    var head2AttnMap = createImageVisAttrs('head2_attention_map', info[ID]['Head2'].Url,  info[ID].width, info[ID].height);
    var head3AttnMap = createImageVisAttrs('head3_attention_map', info[ID]['Head3'].Url,  info[ID].width, info[ID].height);
    var AbMILAttnMap  = createImageVisAttrs('AbMIL_attention_map',  info[ID]['AbMIL'].Url,   info[ID].width, info[ID].height);

	/**
    var viewer = new CurtainSyncViewer({
    container: document.querySelector('#viewer'),
      images: [
        originImage, head0AttnMap, head1AttnMap, head2AttnMap, head3AttnMap, AbMILAttnMap
      ],
    });
	**/

    return [originImage, head0AttnMap, head1AttnMap, head2AttnMap, head3AttnMap, AbMILAttnMap];
}

